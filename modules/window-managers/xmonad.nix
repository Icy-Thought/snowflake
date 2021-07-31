{ config, lib, pkgs, ... }:

let
  xmonadPkgs = with pkgs; [
    haskellPackages.icy-xmonad # xmonad binary
    (rofi.override { plugins = [ rofi-emoji rofi-calc ]; }) # dmenu alt.
    dunst # Notification tool.
    feh # Simple x Image Viewer.
    shotgun # Minimal X Screenshot Util.
    xss-lock # External Locker.
    gxmessage # GTK Drop-in Replacement.
    pavucontrol # PulseAudio Volume Control.
    autorandr # X Resize and Rotate protocol.
    xorg.xkbcomp # Keyboard Codes in X.
  ];

in {
  imports = [ ../../home/xmonad/picom ../display-managers/sddm.nix ];

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
