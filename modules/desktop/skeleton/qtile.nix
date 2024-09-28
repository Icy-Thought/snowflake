{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues optionalAttrs;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.desktop.qtile;
in {
  options.modules.desktop.qtile = let
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) enum;
  in {
    enable = mkEnableOption "python x11/wayland WM";
    backend = mkOption {
      type = enum [ "x11" "wayland" ];
      default = "x11";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      modules.desktop = {
        type = cfg.backend;
        toolset.fileManager = {
          enable = true;
          program = "nautilus";
        };
        extensions = {
          input-method = {
            enable = true;
            framework = "fcitx";
          };
          mimeApps.enable = true; # mimeApps -> default launch application
          picom.enable = mkIf (cfg.backend == "x11");
          dunst.enable = true;
          rofi.enable = true;
        };
      };
      modules.shell.scripts = {
        brightness.enable = true;
        screenshot.enable = true;
      };
      modules.hardware.kmonad.enable = true;

      environment.systemPackages = attrValues ({
        inherit (pkgs) libnotify playerctl gxmessage;
      } // optionalAttrs (cfg.backend == "x11") { inherit (pkgs) xdotool feh; }
        // optionalAttrs (cfg.backend == "wayland") {
          inherit (pkgs) imv wf-recorder;
        });

      services.xserver.windowManager.qtile = {
        enable = true;
        configFile = "${config.snowflake.configDir}/qtile/config.py";
        backend = cfg.backend;
        # extraPackages = attrValues {
        #   inherit (pkgs.python3Packages) qtile-extras;
        # };
      };
    }

    (mkIf (cfg.backend == "wayland") {
      services.greetd.settings.initial_session.command = "none+qtile";

      environment.sessionVariables = {
        ELECTRON_OZONE_PLATFORM_HINT = "auto";
        NIXOS_OZONE_WL = "1";
        MOZ_ENABLE_WAYLAND = "1";
      };
    })

    (mkIf (cfg.backend == "x11") {
      services.displayManager.defaultSession = "none+qtile";
    })
  ]);
}
