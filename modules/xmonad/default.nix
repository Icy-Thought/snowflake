{ config, lib, pkgs, ... }:

let
  xmonadPkgs = with pkgs; [
    (rofi.override { plugins = [ rofi-emoji rofi-calc ]; }) # dmenu alt.
    dunst # Notification tool.
    sxiv # Simple X image viewer.
    hsetroot # Compose wallpaper for X.
    redshift # Screen color temp manager.
    stalonetray # Stand alone tray.
    shotgun # Minimal X screenshot util.
    gxmessage # GTK dropin replacement.
  ];

in {
  environment.systemPackages = builtins.concatLists [ xmonadPkgs ];

  services = {
    blueman.enable = true;

    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      displayManager = { defaultSession = "none+xmonad"; };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;

        extraPackages = haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
          haskellPackages.xmobar
        ];
      };
    };
  };

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
  };

}
