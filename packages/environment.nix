{ config, lib, pkgs, ... }:

let
  envPkgs = with pkgs; [
    mesa # FOSS 3D Graphics Lib.
    vulkan-headers # Header Files + API Registery.
    appimage-run # AppImages Support.
    chrome-gnome-shell # Gnome Shell Integration.
  ];

in { environment.systemPackages = envPkgs; }
