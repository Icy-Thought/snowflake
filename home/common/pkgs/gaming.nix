{ config, lib, pkgs, inputs, ... }:

let
  gamingPkgs = [
    pkgs.lutris # WINE Gaming Platform.
    # pkgs.osu-lazer # FOSS Rythm Game!
  ];

  winePkgs = [
    pkgs.winePackages.staging # Wine-Staging.
    pkgs.winePackages.fonts # MS-fonts by wine-project.
  ];

in { home.packages = gamingPkgs ++ winePkgs; }
