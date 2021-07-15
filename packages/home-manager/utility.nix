{ config, lib, pkgs, ... }:

let
  utilsPkgs = with pkgs; [
    # uutils-coreutils # Rust GNU-coreutils alt.
    wget # TUI Downloader.
    unzip # Unzipping files.
    ffmpeg # Files & streams management.
    pwgen # Password generator.
    imagemagick # LaTeX image export.
    winetricks # Required DLL for exe trouble.
    tree-sitter # Generator + incremental parse.
    ncdu # Dis analyzer (NCurses).
  ];

  tuiPkgs = with pkgs; [
    starship # Minimal + customizable prompt.
    nnn # TUI file manager.
    glances # Curses-based monitoring tool.
    neofetch # Fetch system information.
    youtube-dl # YouTube media downloader.
    # spotify-tui # TUI for premium Spotify users.
    # speedtest-cli # TUI Speedtest.
  ];

in { home.packages = utilsPkgs ++ tuiPkgs; }
