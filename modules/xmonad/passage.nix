{ config, lib, pkgs, ... }: {

  imports = [
    ../home-manager/picom
    ../home-manager/rofi
    ../home-manager/dunst
    ../home-manager/xresources
    ../home-manager/xsession
  ];

  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;

      extraPackages = haskellPackages: [
        haskellPackages.dbus
        haskellPackages.monad-logger
      ];
    };
  };
}
