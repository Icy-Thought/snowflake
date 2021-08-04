{ config, lib, pkgs, ... }:

let fcitx5-nord = (pkgs.callPackage ../../pkgs/fcitx5/fcitx5-nord.nix) { };

in { home.file.".local/share/fcitx5/themes".source = "${fcitx5-nord}"; }
