{ pkgs, ... }:
{
  xsession = {
    enable = true;
    # initExtra = extra + taffybar;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ../config/xmonad/xmonad.hs;

      extraPackages = haskellPackages: [
        haskellPackages.dbus
        haskellPackages.monad-logger
      ];
    };
  };

}
