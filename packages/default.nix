{ config, pkgs, ... }:

let
  cpupower = config.boot.kernelPackages.cpupower;
  perf = config.boot.kernelPackages.perf;

  kernelPkgs = [
    pkgs.cpupower # Examine/Tool Powersaving Features.
    pkgs.perf # Profile & Performance Counter.
  ];

  sysPkgs = [
    pkgs.tree # Tree view.
    pkgs.wireguard # Wireguard Tools.
    pkgs.killall # Completely Eradicate Processes.
    pkgs.xclip # Copy/Paste in Xterm.
    pkgs.wl-clipboard # Wayland c-p/c-v.
  ];

  altPkgs = [
    pkgs.exa # Better ls.
    pkgs.pv # Progress-bar.
    pkgs.fd # Faster Find.
    pkgs.ripgrep # Faster grep.
    pkgs.skim # Faster fzf.
  ];

  utilPkgs = [
    pkgs.gnupg # Encrypt/Decrypt.
    pkgs.firejail # Namespace-based Sandboxing.
    pkgs.exiftool # Control File Metadata.
    pkgs.gh # Official GH Client.
    pkgs.agenix # age-Encrypted Secrets.
  ];

  envPkgs = [
    pkgs.mesa # FOSS 3D Graphics Lib.
    pkgs.vulkan-headers # Header Files + API Registery.
    pkgs.appimage-run # AppImages Support.
  ];

in { environment.systemPackages = sysPkgs ++ altPkgs ++ utilPkgs ++ envPkgs; }
