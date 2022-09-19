{
  inputs,
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.xmonad;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hm.imports = [inputs.hyprland.homeManagerModule];

    environment.systemPackages = with pkgs; [
      feh
      # hyprpaper
      # hyprpicker
      libnotify
      lightdm
      playerctl
      wf-recorder
      wl-clipboard
      wlr-randr
    ];

    # Our beloved modules
    modules.desktop = {
      media.browser.nautilus.enable = true;
      extra = {
        customLayout.enable = true;
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        dunst.enable = true;
        # TODO EWW-bar
        wofi.enable = true;
      };
    };

    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+hyprland";
        lightdm.enable = true;
        lightdm.greeters.mini.enable = true;
      };
      windowManager.hyprland = {
        enable = true;
        systemdIntegration = true;
        xwayland = {
          enable = true;
          hidpi = true;
        };
        extraConfig = builtins.readFile "${configDir}/hyprland/hyprland.conf"; # TODO
      };
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
