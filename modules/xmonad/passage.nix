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
    # initExtra = extra + taffybar;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;

      extraPackages = haskellPackages: [
        haskellPackages.dbus
        haskellPackages.monad-logger
      ];
    };
  };

  home.file = {
    ".xmonad/xmobar.hs" = {
      source = ./xmobar.hs;
      onChange = ''
        if [[ -v DISPLAY ]] ; then
          echo "Recompiling xmobar"
          xmobar -r ~/.xmonad/xmobar.hs &
          sleep 2
          disown
          killall xmobar
          echo "Restarting"
          xmonad --restart
        fi
      '';
    };

    ".xmonad/Theme" = {
      source = ./Theme;
      recursive = true;
    };
  };

}
