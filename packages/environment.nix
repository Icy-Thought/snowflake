{ config, lib, pkgs, ... }:

let
  envPkgs = with pkgs; [
    mesa # FOSS 3D Graphics Lib.
    vulkan-headers # Header files + API registery.
    appimage-run # AppImages support.
    chrome-gnome-shell # Gnome Shell integration.
  ];

in { environment.systemPackages = envPkgs; }
