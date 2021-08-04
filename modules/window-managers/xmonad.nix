{ config, lib, pkgs, ... }:

let
  defaultPkgs = with pkgs; [
    autorandr # X Resize and Rotate protocol.
    xorg.xkbcomp # Keyboard Codes in X.
    xss-lock # External Locker.
    pavucontrol # PulseAudio Volume Control.
    playerctl # Media Player Controller.
    dolphin # Qt File-Manager.
    gxmessage # GTK Drop-in Replacement.
    shotgun # Minimal X Screenshot Util.
    hacksaw # Selection Tool for Screenshot Scripts.
  ];

  xmonadPkgs = with pkgs; [
    haskellPackages.icy-xmonad # xmonad binary
    dunst # Notification tool.
    feh # Simple x Image Viewer.
  ];

in {
  imports = [ ../../home/xmonad/picom ../display-managers/sddm.nix ];

  environment.systemPackages = defaultPkgs ++ xmonadPkgs;

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
