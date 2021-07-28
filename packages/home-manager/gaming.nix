{ config, lib, pkgs, inputs, ... }:

let
  gamingPkgs = with pkgs; [
    lutris # WINE Gaming Platform.
    osu-lazer # FOSS Rythm Game!
  ];

  winePkgs = with pkgs.winePackages; [
    staging # Wine-Staging.
    fonts # MS-fonts by wine-project.
  ];

in { home.packages = gamingPkgs ++ winePkgs; }
