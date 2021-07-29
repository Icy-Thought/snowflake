{ config, lib, pkgs, ... }:

let
  utilsPkgs = with pkgs; [
    # uutils-coreutils # Rust GNU-coreutils Alt.
    wget # TUI Downloader.
    unzip # Unzipping Files.
    ffmpeg # Files & Streams Management.
    pwgen # Password Generator.
    imagemagick # LaTeX Image Export.
    winetricks # Required DLL for Exe Trouble.
    tree-sitter # Generator + Incremental Parse.
    ncdu # Dis Analyzer (NCurses).
  ];

  tuiPkgs = with pkgs; [
    starship # Minimal + Customizable Prompt.
    nnn # TUI File Manager.
    glances # Curses-based Monitoring Tool.
    neofetch # Fetch System Information.
    youtube-dl # YouTube Media Downloader.
    # spotify-tui # TUI for Premium Spotify Users.
    # speedtest-cli # TUI Speedtest.
  ];

in { home.packages = utilsPkgs ++ tuiPkgs; }
