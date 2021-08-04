{ config, lib, pkgs, ... }:

let
  leftPkgs = with pkgs; [
    (rofi.override { plugins = [ rofi-emoji rofi-calc ]; })
    dunst # Notification tool.
    polybar # Customizable toolbar.
    trayer # SysTray
    feh # Image viewer.
    shotgun # Minimal X screenshot util.
  ];

in {
  imports = [ ../../home/leftwm/picom ../display-managers/sddm.nix ];

  environment.systemPackages = leftPkgs;

  services = {
    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      windowManager.leftwm.enable = true;
      displayManager.defaultSession = "none+leftwm";
    };
  };

}
