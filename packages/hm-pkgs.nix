{ config, lib, pkgs, ... }:

let
  sysPkgs = with pkgs; [
    transmission-gtk
    coreutils
    wget
    unzip
    unrar
    zstd
    ffmpeg
    pwgen
    imagemagick
    winetricks
    tree-sitter
    ncdu
  ]; # polychromatic & uutils-coreutils (fix)

  gitPkgs = with pkgs.gitAndTools; [ git-filter-repo git-crypt diff-so-fancy ];

  tuiPkgs = with pkgs; [
    nnn
    glances
    neofetch
    youtube-dl
    # pipes-rs
  ];
  # pkgs.spotify-tui

  dictPkgs = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.sv
    hunspellDicts.sv_SE
    hunspellDicts.en_US
  ];

  nixPkgs = with pkgs; [
    nixfmt
    nixpkgs-fmt
    nix-top
    nixpkgs-review
    crate2nix
    cabal2nix
    any-nix-shell
    nix-direnv
  ];

  devPkgs = with pkgs; [
    languagetool
    gcc11
    gnumake
    cmake
    rust-bin.nightly.latest.default
    ghc
    cabal-install
    stylish-haskell
    sumneko-lua-language-server
    hugo
    openssl
  ]; # nodejs-16_x

  lspPkgs = with pkgs; [
    ccls
    rust-analyzer
    rnix-lsp
    haskellPackages.hoogle
    haskell-language-server
    nodePackages.bash-language-server
  ]; # nodePackages.[ pyright & typescript-language-server ]

  pyPkgs = with pkgs; [
    python39
    pipenv
    python39Packages.black
    python39Packages.isort
    python39Packages.pyflakes
    python39Packages.nose-timer
    python39Packages.nose-exclude
    python39Packages.pytest
  ]; # Disabled

  emacsPkgs = with pkgs; [ graphviz tectonic gnuplot sqlite jq xsv ];

  utilPkgs = with pkgs; [
    gnome.geary
    firefox-devedition-bin
    ueberzug
    anki
    zathura
    foliate
    qalculate-gtk
  ]; # libreoffice & heimdall-gui

  chatPkgs = with pkgs; [ discord tdesktop element-desktop ];
  # signal-desktop & zoom-us

  mediaPkgs = with pkgs; [
    celluloid
    freetube
    gimp
    inkscape
    easytag
  ]; # obs-studio & blender

  gamingPkgs = with pkgs; [ lutris ];
  # osu-lazer

  winePkgs = with pkgs.winePackages; [ staging fonts ];

in {
  home.packages = sysPkgs ++ gitPkgs ++ tuiPkgs ++ dictPkgs ++ nixPkgs
    ++ nixPkgs ++ devPkgs ++ lspPkgs ++ emacsPkgs ++ utilPkgs ++ chatPkgs
    ++ mediaPkgs ++ gamingPkgs ++ winePkgs;
}
