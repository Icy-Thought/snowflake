{ config, pkgs, ... }:
let
  imports = [
    ../../config/plasma.nix
    # ../../config/xmonad.nix
  ];

in { inherit imports; }
