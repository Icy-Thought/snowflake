{ config, lib, pkgs, ... }:

let
  gitPkgs = with pkgs.gitAndTools; [
    diff-so-fancy # Colored git diff.
    git-crypt # git File Encryption.
    tig # diff & commit View.
  ];

  dictPkgs = with pkgs; [
    aspell # Spelling Support.
    aspellDicts.en # en_US Aspell.
    aspellDicts.sv # sv_SE Aspell.
    hunspellDicts.sv_SE # sv_SE Hunspell.
    hunspellDicts.en_US # en_US Hunspell.
  ];

  nixPkgs = with pkgs; [
    any-nix-shell # Fish/ZSH Support.
    nix-direnv # Fast nix-impl of direnv.
  ];

in { home.packages = gitPkgs ++ dictPkgs ++ nixPkgs; }
