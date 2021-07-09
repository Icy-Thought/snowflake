{ config, lib, pkgs, ... }:

let
  xmonadPkgs = with pkgs; [
    xmobar # text-based statusbar.
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
        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: rec {
            xmonad = let
              pkg = hself.callPackage
                "${config.home.homeDirectory}/git/Snowflake/overlays/xmonad"
                { };
            in pkgs.haskell.lib.dontCheck pkg;
            xmonad-extras = hself.callPackage
              "${config.home.homeDirectory}/git/Snowflake/overlays/xmonad-extras"
              { };
            xmonad-contrib = let
              pkg = hself.callPackage
                "${config.home.homeDirectory}/git/Snowflake/overlays/xmonad-contrib"
                { };
            in pkgs.haskell.lib.dontCheck pkg;
          };
        };
      };
    };
  };

}
