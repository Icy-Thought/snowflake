{ config, lib, pkgs, ... }:

let
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

in { environment.systemPackages = utilPkgs ++ sysPkgs ++ altPkgs; }
