{ config, lib, pkgs, ... }:

let
  editorPkgs = with pkgs; [
    gnuplot # Plotting through programming.
    tmux # Terminal multiplexer.
  ];

in { home.packages = editorPkgs; }
