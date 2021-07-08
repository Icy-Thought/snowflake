{ config, lib, pkgs, ... }:

let

  xmonadPkgs = with pkgs; [
    (haskellPackages.ghcWithPackages
      (self: with self; [ xmonad xmonad-extras xmonad-contrib xmobar ]))
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
      };
    };
  };
}
