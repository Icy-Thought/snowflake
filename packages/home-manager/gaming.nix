{ config, lib, pkgs, inputs, ... }:

let
  gamingPkgs = with pkgs; [
    wine-staging # Latest Wine package.
    lutris # WINE gaming platform.
    osu-lazer # FOSS Rythm game!
  ];

in { home.packages = gamingPkgs; }
