{ config, lib, pkgs, ... }:

let

  xmonad-contrib = with pkgs;
    haskell.lib.overrideCabal haskellPackages.xmonad-contrib (old: {
      src = fetchFromGitHub {
        owner = "xmonad";
        repo = "xmonad-contrib";
        rev = "master";
        sha256 = "";
      };
    });

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

        extraPackages = hsPkgs: [
          hsPkgs.xmonad
          hsPkgs.xmonad-extras
          hsPkgs.xmobar
          xmonad-contrib
        ];
      };
    };
  };
}
