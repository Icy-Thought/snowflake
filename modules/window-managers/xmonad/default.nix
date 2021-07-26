{ config, lib, pkgs, ... }:

let
  xmonadPkgs = with pkgs; [
    cabal-install # TUI for Cabal & Haskell.
    (rofi.override { plugins = [ rofi-emoji rofi-calc ]; }) # dmenu alt.
    dunst # Notification tool.
    sxiv # Simple X image viewer.
    hsetroot # Compose wallpaper for X.
    gammastep # Screen color temp manager.
    stalonetray # Stand alone tray.
    shotgun # Minimal X screenshot util.
    scrot # CLI screen-capture util.
    gxmessage # GTK dropin replacement.
  ];

in {
  environment.systemPackages = xmonadPkgs;

  services = {
    blueman.enable = true;

    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      windowManager.xmonad.enable = true;
      displayManager.defaultSession = "none+xmonad";
    };
  };

}
