{ inputs, options, config, lib, pkgs, ... }:

let
  hyprDir = "${config.snowflake.configDir}/hyprland";
  pyprland = inputs.pyprland.packages."x86_64-linux".pyprland;
in with lib; {
  options.modules.desktop.hyprland = {
    enable = mkEnableOption "hyped wayland WM";
  };

  config = mkIf config.modules.desktop.hyprland.enable {
    modules.desktop = {
      type = "wayland";
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
        dunst.enable = true;
        waybar.enable = true;
        rofi.enable = true;
      };
    };
    modules.shell.scripts = {
      brightness.enable = true;
      screenshot.enable = true; # TODO
    };
    modules.hardware.kmonad.enable = true;

    environment.systemPackages = with pkgs; [
      imv
      libnotify
      playerctl
      wf-recorder
      wlr-randr
      pyprland
    ];

    environment.sessionVariables = {
      ELECTRON_OZONE_PLATFORM_HINT = "auto";
      NIXOS_OZONE_WL = "1";
      MOZ_ENABLE_WAYLAND = "1";
    };

    programs.hyprland.enable = true;
    hm.wayland.windowManager.hyprland = {
      enable = true;
      xwayland.enable = true;
      settings.source = [
        "${hyprDir}/constants.conf"
        "${hyprDir}/main.conf"
        "${hyprDir}/interface.conf"
        "${hyprDir}/rules.conf"
        "${hyprDir}/keymaps.conf"
      ];
    };

    hm.systemd.user.services.pyprland = {
      Unit = {
        Description = "An extension package for Hyprland.";
        PartOf = [ "hyprland-session.target" ];
        After = [ "hyprland-session.target" ];
        ConditionEnvironment = "WAYLAND_DISPLAY";
      };
      Service = {
        Type = "simple";
        ExecStart = "${pyprland}/bin/pypr";
        ExecStop = "${
            lib.getExe pkgs.bash
          } -c 'rm $XDG_RUNTIME_DIR/hypr/*/.pyprland.sock'";
        Restart = "always";
      };
      Install.WantedBy = [ "hyprland-session.target" ];
    };

    create.configFile.pyprland-conf = {
      target = "hypr/pyprland.toml";
      source = "${hyprDir}/pyprland.toml";
    };
    # hypridle? hyprlock?
  };
}
