{ config, lib, pkgs, ... }:

let
  leftPkgs = [
    (pkgs.rofi.override { plugins = [ pkgs.rofi-emoji pkgs.rofi-calc ]; })
    pkgs.dunst # Notification tool.
    pkgs.polybar # Customizable toolbar.
    pkgs.trayer # SysTray
    pkgs.feh # Image viewer.
    pkgs.shotgun # Minimal X screenshot util.
  ];

in {
  imports = [ ../../config/picom/leftwm.nix ../display-managers/sddm.nix ];

  environment.systemPackages = leftPkgs;

  services = {
    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      windowManager.leftwm.enable = true;
      displayManager.defaultSession = "none+leftwm";
    };
  };

}
