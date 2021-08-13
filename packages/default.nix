{ config, pkgs, ... }:

let
  cpupower = config.boot.kernelPackages.cpupower;
  perf = config.boot.kernelPackages.perf;

  kernelPkgs = with pkgs; [ cpupower perf ];
  # Disabled

  sysPkgs = with pkgs; [ tree wireguard killall xclip wl-clipboard ];

  altPkgs = with pkgs; [ exa pv fd ripgrep skim ];

  utilPkgs = with pkgs; [ gnupg firejail exiftool gh agenix ];

  envPkgs = with pkgs; [ mesa vulkan-headers appimage-run ];

in { environment.systemPackages = sysPkgs ++ altPkgs ++ utilPkgs ++ envPkgs; }
