{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.profiles.dev;

  deskPack = with pkgs; [
    # pop-os-shell                                      # Gnome Tiling Manager.
    zathura                                             # Minimal PDF/EPUB/DJUV/.. reader.
    alacritty                                           # Rust-based Terminal.
    brave                                               # Chromium-based browser.
    latest.firefox-nightly-bin                          # Latest Firefox edition.
    openrazer-daemon                                    # Control your razer hardware.
    # Polychromatic                                     # GUI to control OpenRazer.
  ];

  appPack = with pkgs; [
    mpv                                                 # Media Player.
    celluloid                                           # GTK frontend for MPV.
    gimp                                                # The better Photoshop alternative.
    inkscape                                            # The better Illustrator alternative.
    libreoffice                                         # The better office alternative suit.
    obs-studio                                          # Streaming/Recording software.
    qalculate-gtk                                       # Scientific calculator.
    discord-canary                                      # Latest Discord client.
    tdesktop                                            # Telegram Desktop.
    signal-desktop                                      # More secure WhatsApp alternative.
    fractal                                             # Rust-based matrix client.
    # gnome.geary                                         # Gnome 2nd E-Mail client.
    gnome.polari                                      # Gnome IRC client.
    easytag                                             # M3U Editor.
    transmission-gtk                                    # BitTorrent Client.
    freetube                                            # FOSS private YT app.
    foliate                                             # Minimal E-Book reader.
    zoom-us                                             # Conferencing application..
  ];

  langPack = with pkgs; [
    latest.rustChannels.nightly.rust                    # Rust compiler.
    unstable.rust-analyzer                              # Rust-Analyzer for better rust completion.
    languagetool                                        # Proofreading (grammar checker) for several languages.
    ccls                                                # C/C++ language server powered by clang.
    cmake                                               # Automated build automation, testing & packaging software.
    texlive.combined.scheme-medium                      # LaTeX support.
    nodePackages.npm                                    # JS package manager.
    nodePackages.javascript-typescript-langserver       # Javascript support.
    podman                                              # Docker alternative.
    hugo                                                # Fast + Modern static web-engine.
  ];

  editorPack = with pkgs; [
    doom-emacs                                          # Emacs + Doom support.
    gnuplot                                             # Plotting through programming.
    neovim-nightly                                      # Neovim + Lua support.
    tmux                                                # Terminal multiplexer.
  ];

  gamePack = with pkgs; [
    wine-staging                                        # Latest Wine package.
    steam                                               # Gaming platform.
    lutris                                              # WINE gaming platform.
    osu-lazer                                           # FOSS Rythm game!
  ];

  gitPack = with pkgs; [
    diff-so-fancy
    git-crpt
    hub
    tig
  ];

  gnomePack = with pkgs; [
    gnome.gnome-tweak-tool                              # Advance Gnome setting control.
    # gnomeExtensions.appindicator                      # Enables systray icons.
    # gnomeExtensions.gsconnect                         # KDE Connect for Gnome.
  ];

  spellPack = with pkgs; [
    aspellDicts.en
    aspellDicts.sv
    hunspellDicts.sv_SE
    hunspellDicts.en_US
    ibus                                                # Support for CJK language input.
    ibus-engines.libpinyin
    libpinyin
  ];

  nixPack = with pkgs; [
    hydra-check
    nix-prefetch-github
    nixpkgs-review
    nix-top
    nixpkgs-fmt
  ];

  tuiPack = with pkgs; [
    neofetch                                           # Fetch system information.
    htop                                               # Monitor current processes.
    # mpd                                                # Media player daemon.
    # ncmpcpp                                            # TUI music player.
    # spotify-tui                                        # TUI for premium Spotify users.
    # speedtest-cli                                      # TUI Speedtest.
    youtube-dl                                         # YouTube media downloader.
    nnn                                                # TUI file manager.
  ];
    
  utilsPack = with pkgs; [
    any-nix-shell                                      # Fish/ZSH support for nix-shell.
    uutils-coreutils                                   # Rust Rewrite of GNU-coreutils.
    gnupg                                              # Encrypt/Decrypt software.
    wget                                               # Downloading files from URL in terminal.
    unzip                                              # Enable unzipping files.
    unrar                                              # Enable extracting files from rar files.
    xclip                                              # Copy/Paste in XOrg terminal.
    wl-clipboard                                       # Copy/Paste in wayland terminal.
    gnumake                                            # Controls the generation of executable files.
    ffmpeg                                             # Library + Programs for management of multimedia files and streams.
    pwgen                                              # Password generator.
    ripgrep                                            # faster grep.
    skim                                               # faster fzf.
    exa                                                # better ls.
    fd                                                 # faster find.
    pv                                                 # Progress-bar for mv/cp.
    upower                                             # D-Bus service for power management.
    zstd                                               # Undo-fu-session/undo-tree-compression.
    imagemagick                                        # LaTeX image export.
    winetricks                                         # Install required DLL to escape exe trouble.
    tree-sitter                                        # Parser generator + incremental parsing lib.
    starship                                           # Minimal + customizable prompt.
  ];

in {
  config = lib.mkIf cfg.enable {
    home.packages = builtins.concatLists [
       deskPack
       appPack
       editorPack
       gamePack
       langPack
       spellPack
       gnomePack
       nixPack
       tuiPack
       utilsPack
    ];
  };

}
