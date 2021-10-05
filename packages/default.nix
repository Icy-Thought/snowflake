{ config, pkgs, ... }:

let
  cpupower = config.boot.kernelPackages.cpupower;
  perf = config.boot.kernelPackages.perf;

  kernelPkgs = with pkgs; [ cpupower perf ];
  # Disabled

  sysPkgs = with pkgs; [ wireguard killall xclip wl-clipboard ];

  altPkgs = with pkgs; [ exa fd ripgrep skim ];

  utilPkgs = with pkgs; [ gnutls firejail exiftool agenix ];

in { environment.systemPackages = sysPkgs ++ altPkgs ++ utilPkgs; }
