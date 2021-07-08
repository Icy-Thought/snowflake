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
        haskellPackages = with pkgs; [
          haskellPackages.xmobar
          (haskellPackages.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              xmonad = pkgs.haskell.lib.dontCheck haskellPackagesNew.xmonad;
              xmonad-extras = haskellPackagesNew.lib.dontCheck (xmonad-extras);
              xmonad-contrib =
                haskellPackagesNew.lib.dontCheck (xmonad-contrib);
            };
          })
        ];
      };
    };
  };
}
