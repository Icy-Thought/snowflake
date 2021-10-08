{ config, lib, pkgs, ... }:

let
  sysPkgs = with pkgs; [
    transmission-gtk
    pwgen
  ]; # polychromatic & uutils-coreutils (fix)

  tuiPkgs = with pkgs; [ youtube-dl pipes-rs ];

  dictPkgs = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.sv
    hunspellDicts.sv_SE
    hunspellDicts.en_US
  ];

  nixPkgs = with pkgs; [ nix-top any-nix-shell ];

  devPkgs = with pkgs; [ nodejs openssl ];

  utilPkgs = with pkgs; [
    bitwarden
    gnome.geary
    qalculate-gtk
    anki
    spotify
    firefox-devedition-bin
  ]; # libreoffice & heimdall-gui

  chatPkgs = with pkgs; [ master.discord tdesktop element-desktop ];
  # signal-desktop & zoom-us

in { }
