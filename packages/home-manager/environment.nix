{ config, lib, pkgs, ... }:

let
  gitPkgs = with pkgs.gitAndTools; [
    diff-so-fancy # Colored git diff.
    git-crypt # git file encryption.
    tig # diff and commit view.
  ];

  dictPkgs = with pkgs; [
    aspell # Spelling support.
    aspellDicts.en # en_US aspell.
    aspellDicts.sv # sv_SE aspell.
    hunspellDicts.sv_SE # sv_SE hunspell.
    hunspellDicts.en_US # en_US hunspell.
  ];

  nixPkgs = with pkgs; [
    any-nix-shell # Fish/ZSH support.
    nix-direnv # Fast nix-impl of direnv.
    hydra-check # Hydra build status check.
    nix-prefetch-github # Prefetch from GH.
    nixpkgs-review # Review nixpkgs PR.
    nix-top # Tracks nix builds.
    nixfmt # Nix code formatter.
    nixpkgs-fmt # [...] -> for nixpkgs.
    lorri # Project's nix-env.
  ];

in { home.packages = gitPkgs ++ dictPkgs ++ nixPkgs; }
