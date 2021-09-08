{ config, pkgs, ... }:

let
  imports = [
    # ../../config/gnome.nix
    ../../config/xmonad.nix
  ];

in { inherit imports; }
