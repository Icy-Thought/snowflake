{ config, lib, pkgs, ... }:

let
  imports =
    [ ../nixos/fcitx5.nix ../display-managers/sddm.nix ../../config/picom ];

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
