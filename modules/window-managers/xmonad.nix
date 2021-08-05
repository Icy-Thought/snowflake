{ config, lib, pkgs, ... }:

let
  defaultPkgs = [
    pkgs.autorandr # X Resize and Rotate protocol.
    pkgs.xorg.xkbcomp # Keyboard Codes in X.
    pkgs.xss-lock # External Locker.
    pkgs.pavucontrol # PulseAudio Volume Control.
    pkgs.playerctl # Media Player Controller.
    pkgs.dolphin # Qt File-Manager.
    pkgs.gxmessage # GTK Drop-in Replacement.
    pkgs.shotgun # Minimal X Screenshot Util.
    pkgs.hacksaw # Selection Tool for Screenshot Scripts.
    pkgs.dconf # Gsettings-Manager.
    pkgs.hicolor-icon-theme # Fallback-theme TaffyBar.
  ];

  xmonadPkgs = [
    pkgs.haskellPackages.icy-xmonad # xmonad binary
    pkgs.dunst # Notification tool.
    pkgs.feh # Simple x Image Viewer.
  ];

in {
  imports = [ ../../home/xmonad/picom ../display-managers/sddm.nix ];

  environment.systemPackages = defaultPkgs ++ xmonadPkgs;

  services = {
    blueman.enable = true;

    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      displayManager.defaultSession = "none+xmonad";

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
