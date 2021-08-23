{ config, lib, pkgs, ... }:

let
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
  imports = [ ../../config/picom ../display-managers/sddm.nix ];

  environment.systemPackages = defaultPkgs ++ xmonadPkgs;

  gtk.iconCache.enable = true;

  services = {
    blueman.enable = true;

    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      displayManager.defaultSession = "none+xmonad";
      desktopManager.xterm.enable = false;

      # 2-Step workaround for https://github.com/taffybar/taffybar/issues/403
      # 1. Causes GDK_PIXBUF_MODULE_FILE to be set in xsession.
      gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
      displayManager.sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH

        # (QT -> GTK): GTK2_RC_FILES must be available to the displayManager.
        export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
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
