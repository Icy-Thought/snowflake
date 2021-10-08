{ config, lib, pkgs, ... }:

let imports = [ ../nixos/fcitx5.nix ../config/picom ];

in { inherit imports; }
