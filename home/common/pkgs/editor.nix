{ config, lib, pkgs, ... }:

let
  editorPkgs = with pkgs; [
    gnuplot # Plotting Through Programming.
    tmux # Terminal Multiplexer.
  ];

in { home.packages = editorPkgs; }
