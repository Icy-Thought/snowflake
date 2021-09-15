{ config, lib, pkgs, ... }:

let
  imports = [ ../../config/picom ../display-managers/sddm.nix ];

  leftPkgs = with pkgs; [ dunst polybar trayer feh shotgun ];

in {
  inherit imports;

  environment.systemPackages = leftPkgs;

  services = {
    xserver = {
      xkbOptions = "ctrl:swapcaps_hyper,shift:both_capslock";
      windowManager.leftwm.enable = true;
      displayManager.defaultSession = "none+leftwm";
    };
  };

}
