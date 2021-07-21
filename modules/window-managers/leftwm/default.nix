{ config, lib, pkgs, ... }:

let
  leftPkgs = with pkgs; [
    (nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })
    (rofi.override { plugins = [ rofi-emoji rofi-calc ]; }) # Window switcher.
    dunst # Notification tool.
    polybar # Customizable toolbar.
    trayer # SysTray
    feh # Image viewer.
  ];

in {
  imports = [ ./config/picom ];
  environment.systemPackages = leftPkgs;

  services = {
    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      windowManager.leftwm.enable = true;
      displayManager.defaultSession = "none+leftwm";
    };
  };

}
