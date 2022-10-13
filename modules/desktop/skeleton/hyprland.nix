{ inputs
, options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.xmonad.enable {
    modules.desktop = {
      envProto = "wayland";
      toolset.fileBrowse = {
        nautilus.enable = true;
      };
      extensions = {
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        dunst.enable = true;
        # TODO EWW-bar
        rofi = {
          enable = true;
          package = pkgs.rofi-wayland;
        };
        waybar.enable = true;
      };
    };

    hm.imports = [ inputs.hyprland.homeManagerModule ];

    environment.systemPackages = with pkgs; [
      imv
      # hyprpaper
      # hyprpicker
      libnotify
      playerctl
      wf-recorder
      wl-clipboard
      wlr-randr
      wireplumber
    ];

    environment.variables = {
      MOZ_ENABLE_WAYLAND = "1"; # (Firefox) wayland awareness!
    };

    services.xserver = {
      enable = true;
      displayManager.defaultSession = "hyprland";
    };

    hm.wayland.windowManager.hyprland = {
      enable = true;
      systemdIntegration = true;
      xwayland = {
        enable = true;
        hidpi = true;
      };
      extraConfig = builtins.readFile "${config.snowflake.configDir}/hyprland/hyprland.conf"; # TODO
    };

    hm.services = {
      gnome-keyring.enable = true;
      network-manager-applet.enable = true;
      status-notifier-watcher.enable = true;
    };

    hm.xsession = {
      enable = true;
      numlock.enable = true;
      preferStatusNotifierItems = true;
    };
  };
}
