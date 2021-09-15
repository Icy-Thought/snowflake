{ config, lib, pkgs, ... }:

let
  imports = [ ../../config/picom ../display-managers/sddm.nix ];

  defaultPkgs = with pkgs; [
    autorandr
    xorg.xkbcomp
    betterlockscreen
    pavucontrol
    playerctl
    gnome.nautilus
    gxmessage
    shotgun
    hacksaw
    xdotool
    dconf
    hicolor-icon-theme
  ];

  xmonadPkgs = with pkgs; [ haskellPackages.icy-xmonad dunst feh ];

in {
  inherit imports;

  environment.systemPackages = defaultPkgs ++ xmonadPkgs;

  gtk.iconCache.enable = true;

  services = {
    blueman.enable = true;

    xserver = {
      xkbOptions = "ctrl:swapcaps_hyper,shift:both_capslock";
      displayManager.defaultSession = "none+xmonad";

      # 2-Step workaround for https://github.com/taffybar/taffybar/issues/403
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
