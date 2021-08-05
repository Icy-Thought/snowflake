{ config, lib, pkgs, ... }:

let
  utilsPkgs = [
    # pkgs.uutils-coreutils # Rust GNU-coreutils Alt.
    pkgs.wget # TUI Downloader.
    pkgs.unzip # Unzipping Files.
    pkgs.unrar # File Uncompressor.
    pkgs.zstd # Undo fu-Session/tree-Compression.
    pkgs.ffmpeg # Files & Streams Management.
    pkgs.pwgen # Password Generator.
    pkgs.imagemagick # LaTeX Image Export.
    pkgs.winetricks # Required DLL for Exe Trouble.
    pkgs.tree-sitter # Generator + Incremental Parse.
    pkgs.ncdu # Dis Analyzer (NCurses).
  ];

  tuiPkgs = [
    pkgs.nnn # TUI File Manager.
    pkgs.glances # Curses-based Monitoring Tool.
    pkgs.neofetch # Fetch System Information.
    pkgs.youtube-dl # YouTube Media Downloader.
    # pkgs.spotify-tui # TUI for Premium Spotify Users.
    # pkgs.speedtest-cli # TUI Speedtest.
  ];

in { home.packages = utilsPkgs ++ tuiPkgs; }
