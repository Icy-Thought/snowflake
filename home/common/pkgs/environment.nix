{ config, lib, pkgs, ... }:

let
  gitPkgs = [
    pkgs.gitAndTools.diff-so-fancy # Colored git diff.
    pkgs.gitAndTools.git-crypt # git File Encryption.
    pkgs.gitAndTools.tig # diff & commit View.
  ];

  dictPkgs = [
    pkgs.aspell # Spelling Support.
    pkgs.aspellDicts.en # en_US Aspell.
    pkgs.aspellDicts.sv # sv_SE Aspell.
    pkgs.hunspellDicts.sv_SE # sv_SE Hunspell.
    pkgs.hunspellDicts.en_US # en_US Hunspell.
  ];

  nixPkgs = [
    pkgs.any-nix-shell # Fish/ZSH Support.
    pkgs.nix-direnv # Fast nix-impl of direnv.
  ];

in { home.packages = gitPkgs ++ dictPkgs ++ nixPkgs; }
