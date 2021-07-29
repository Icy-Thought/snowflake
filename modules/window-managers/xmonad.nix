{ config, lib, pkgs, ... }:

let
  xmonadPkgs = with pkgs; [
    haskellPackages.icy-xmonad # xmonad binary
    (rofi.override { plugins = [ rofi-emoji rofi-calc ]; }) # dmenu alt.
    dunst # Notification tool.
    feh # Simple x Image Viewer.
    sxiv # Simple X image viewer.
    gammastep # Screen Color Temp-Manager.
    stalonetray # Stand-alone Tray.
    shotgun # Minimal X Screenshot Util.
    scrot # CLI Screen-Capture Util.
    xss-lock # External Locker.
    gxmessage # GTK Drop-in Replacement.
  ];

in {
  imports = [ ../../home/xmonad/picom ];
  environment.systemPackages = xmonadPkgs;

  services.xserver = {
    xkbOptions = "caps:ctrl_modifier";
    displayManager.defaultSession = "none+xmonad";

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

}
