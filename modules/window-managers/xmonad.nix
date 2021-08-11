{ config, lib, pkgs, ... }:

let
  defaultPkgs = [
    pkgs.autorandr # X resize and rotate protocol.
    pkgs.xorg.xkbcomp # Keyboard codes in X.
    pkgs.betterlockscreen # Minimal lockscreen.
    pkgs.pavucontrol # PulseAudio volume control.
    pkgs.playerctl # Media player controller.
    pkgs.dolphin # Qt file-manager.
    pkgs.gxmessage # GTK drop-in replacement.
    pkgs.shotgun # Minimal X screenshot util.
    pkgs.hacksaw # Selection tool for screenshot scripts.
    pkgs.dconf # Gsettings-manager.
    pkgs.hicolor-icon-theme # Fallback-theme TaffyBar.
  ];

  xmonadPkgs = [
    pkgs.haskellPackages.icy-xmonad # xmonad binary
    pkgs.dunst # Notification tool.
    pkgs.feh # Simple x Image Viewer.
  ];

in {
  imports = [ ../../config/picom/xmonad.nix ../display-managers/sddm.nix ];

  environment.systemPackages = defaultPkgs ++ xmonadPkgs;

  gtk.iconCache.enable = true;

  services = {
    blueman.enable = true;

    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      displayManager.defaultSession = "none+xmonad";

      # Workaround https://github.com/taffybar/taffybar/issues/403
      # 1. Causes GDK_PIXBUF_MODULE_FILE to be set in xsession.
      gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
      displayManager.sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
      '';

      windowManager = {
        session = [{
          name = "xmonad";
          start = ''
            /usr/bin/env icy-xmonad &
            waitPID=$!
          '';
        }];
      };
    };
  };

}
