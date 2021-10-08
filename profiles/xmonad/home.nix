{ config, lib, pkgs, ... }:

let
  imports = [
    ./qt
    ./gtk
    ./rofi
    ./dunst
    ./xresources
    ./scripts/brightness.nix
    ./scripts/screenshot.nix
    ./scripts/volume.nix
    ./scripts/microphone.nix
    # ./scripts/screen-recording.nix
  ];

in { inherit imports; }
