{ config, lib, pkgs, ... }:

let leftPkgs = with pkgs; [ dunst polybar trayer feh shotgun ];

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
