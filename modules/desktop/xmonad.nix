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
  options.modules.desktop.xmonad = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = with inputs; [
      xmonad.overlay
      xmonad-contrib.overlay
    ];

    environment.systemPackages = with pkgs; [
      haskellPackages.my-xmonad
      lightdm
      libnotify
      dunst
      pavucontrol
      playerctl
      gxmessage
      xdotool
      xclip
      feh
    ];

    modules.desktop.media.browser = {
      nautilus.enable = true;
      # thunar.enable = true;
    };

    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+xmonad";
        lightdm = {
          enable = true;
          greeters.mini.enable = true;
        };
      };

      windowManager.session = [
        {
          name = "xmonad";
          start = ''
            /usr/bin/env my-xmonad &
            waitPID=$!
          '';
        }
      ];
    };

    services = {
      autorandr.enable = true;
      blueman.enable = true;
    };

    # Xdg.mimeApps -> application management
    modules.desktop.extra.mimeApps.enable = true;

    home.services = {
      gnome-keyring.enable = true;
      blueman-applet.enable = true;
      status-notifier-watcher.enable = true;
      network-manager-applet.enable = true;
    };

    home.xsession = {
      enable = true;
      numlock.enable = true;
      preferStatusNotifierItems = true;
      windowManager.command = "${getExe pkgs.haskellPackages.my-xmonad}";
      importedVariables = ["GDK_PIXBUF_MODULE_FILE"];
    };
  };
}
