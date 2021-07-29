{ config, lib, pkgs, ... }:

let
  xmonad = ''
    userresources = "${config.xdg.configHome}"/x11/Xresources
    [ -f "$userresources" ] && xrdb -merge "$userresources"

    # Firefox-related:
    export MOZ_X11_EGL=1
    export XDG_SESSION_TYPE=x11
  '';

in {
  imports =
    [ ./xresources ./screen-locker ./dunst ./rofi ./stalonetray ./scripts ];

  config.services = {
    gnome-keyring.enable = true;
    network-manager-applet.enable = true;
    blueman-applet.enable = true;

    random-background = {
      enable = true;
      display = "fill";
      imageDirectory = "%h/Pictures/Wallpapers/Randomize";
    };
  };

  config.xsession = {
    enable = true;
    numlock.enable = true;
    initExtra = xmonad;
    windowManager.command = ''
      ${pkgs.haskellPackages.icy-xmonad}/bin/icy-xmonad
    '';
  };
}
