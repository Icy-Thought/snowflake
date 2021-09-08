{ config, lib, pkgs, ... }:

let
  imports = [
    ./brightness.nix
    ./screenshot.nix
    ./volume.nix
    ./microphone.nix
    # ./screen-recording.nix
  ];

in { inherit imports; }
