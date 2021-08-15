{ config, lib, pkgs, ... }:

let
  defaultPkgs = with pkgs; [
    autorandr
    xorg.xkbcomp
    betterlockscreen
    pavucontrol
    playerctl
    dolphin
    gxmessage
    shotgun
    hacksaw
    dconf
    hicolor-icon-theme
  ];

  xmonadPkgs = with pkgs; [ haskellPackages.icy-xmonad dunst feh ];

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
