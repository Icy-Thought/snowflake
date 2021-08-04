{ config, lib, pkgs, ... }:

let
  envPkgs = [
    pkgs.mesa # FOSS 3D Graphics Lib.
    pkgs.vulkan-headers # Header Files + API Registery.
    pkgs.appimage-run # AppImages Support.
  ];

in { environment.systemPackages = envPkgs; }
