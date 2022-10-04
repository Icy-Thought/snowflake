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
    hm.imports = [ inputs.hyprland.homeManagerModule ];

    environment.systemPackages = with pkgs; [
      imv
      # hyprpaper
      # hyprpicker
      libnotify
      lightdm
      playerctl
      wf-recorder
      wl-clipboard
      wlr-randr
      wireplumber
    ];

    environment.variables = {
      MOZ_ENABLE_WAYLAND = "1"; # (Firefox) wayland awareness!
    };

    # Our beloved modules
    modules.desktop = {
      media.browser.nautilus.enable = true;
      extra = {
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

    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+hyprland";
        lightdm.enable = true;
        lightdm.greeters.mini.enable = true;
      };
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
