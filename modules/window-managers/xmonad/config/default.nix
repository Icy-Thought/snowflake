{ config, lib, pkgs, ... }:

{
  imports =
    [ ./dunst ./rofi ./xmobar ./xsession ./picom ./scripts ./xresources ];

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad/config.hs;
    libFiles = {
      "Theme.hs" = ./xmonad/lib/Theme/Theme.hs;
      "Xresources.hs" = ./xmonad/lib/Theme/Xresources.hs;
    };

    extraPackages = haskellPackages:
      with haskellPackages; [
        dbus
        monad-logger
        greenclip
      ];
  };
}
