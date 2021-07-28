{ config, lib, pkgs, ... }:

{
  imports = [ ./dunst ./rofi ./xsession ./picom ./scripts ./xresources ];

  xsession.windowManager.xmonad = {
    enable = true;

    extraPackages = haskellPackages:
      with haskellPackages; [
        dbus
        monad-logger
        greenclip
      ];
  };
}
