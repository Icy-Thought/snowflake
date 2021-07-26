{ config, lib, pkgs, inputs, ... }:

let
  gamingPkgs = with pkgs; [
    wine-staging # Latest Wine Package.
    lutris # WINE Gaming Platform.
    osu-lazer # FOSS Rythm Game!
  ];

in { home.packages = gamingPkgs; }
