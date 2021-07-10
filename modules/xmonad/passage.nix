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
      config = lib.mkDefault ./haskell/xmonad.hs;

      libFiles = {
        "Theme.hs" = ./haskell/Theme/Theme.hs;
        "Xresources.hs" = ./haskell/Theme/Xresources.hs;
      };

      extraPackages = haskellPackages: [
        haskellPackages.dbus
        haskellPackages.monad-logger
      ];
    };
  };

  xdg.configFile."xmonad/xmobar.hs" = {
    source = ./haskell/xmobar.hs;
    onChange = ''
      if [[ -v DISPLAY ]]; then
          echo "Restarting xmonad"
          $DRY_RUN_CMD ${config.xsession.windowManager.command} --restart
      fi
    '';
  };
}
