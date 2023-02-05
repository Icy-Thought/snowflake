{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption;
  inherit (lib.strings) optionalString;
  inherit (lib.types) enum nullOr path package;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.qtile;
in {
  options.modules.desktop.qtile = {
    enable = mkBoolOpt false;
    package = mkOption {
      type = package;
      default = pkgs.qtile;
    };

    configFile = mkOption {
      type = nullOr path;
      default = "${config.snowflake.configDir}/qtile/config.py";
      example = "./config.py";
    };

    backend = mkOption {
      type = enum [ "x11" "wayland" ];
      default = "x11";
    };
  };

  config = mkIf cfg.enable {
    modules.desktop = {
      envProto = cfg.backend;
      toolset.fileBrowse = { nautilus.enable = true; };
      extensions = {
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        picom.enable = true;
        dunst.enable = true;
        rofi.enable = true;
      };
    };
    modules.hardware.kmonad.enable = true;

    environment.systemPackages = with pkgs; [
      (cfg.package.unwrapped or cfg.package)
      libnotify
      playerctl
      gxmessage
      xdotool
      xclip
      feh
    ];

    services.xserver = {
      displayManager.defaultSession = "none+qtile";
      windowManager.session = [{
        name = "qtile";
        start = ''
          ${cfg.package}/bin/qtile start -b ${cfg.backend} \
          ${
            optionalString (cfg.configFile != null)
            "--config ${cfg.configFile} "
          } & waitPID=$!
        '';
      }];
    };
  };
}
