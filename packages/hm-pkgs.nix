{ config, lib, pkgs, ... }:

let
  sysPkgs = with pkgs; [
    transmission-gtk
    wget
    unzip
    unrar
    zstd
    ncdu
    ffmpeg
    pwgen
    imagemagick
  ]; # polychromatic & uutils-coreutils (fix)

  gitPkgs = with pkgs.gitAndTools; [
    gh
    gist
    git-crypt
    git-open
    git-filter-repo
    diff-so-fancy
  ];

  tuiPkgs = with pkgs; [ neofetch youtube-dl pipes-rs ];

  dictPkgs = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.sv
    hunspellDicts.sv_SE
    hunspellDicts.en_US
  ];

  nixPkgs = with pkgs; [
    nixfmt
    nix-top
    crate2nix
    cabal2nix
    any-nix-shell
    nix-direnv
  ];

  devPkgs = with pkgs; [
    languagetool
    gcc
    gnumake
    cmake
    nodejs
    rust-bin.beta.latest.default
    ghc
    cabal-install
    stylish-haskell
    hugo
    openssl
  ];

  lspPkgs = with pkgs; [
    ccls
    rust-analyzer
    haskellPackages.hoogle
    haskell-language-server
    nodePackages.bash-language-server
  ]; # nodePackages.javascript-typescript-langserver

  pyPkgs = with pkgs; [
    python39
    python39Packages.pip
    python39Packages.ipython
    python39Packages.black
    python39Packages.setuptools
    python39Packages.pylint
    python39Packages.poetry
  ];

  emacsPkgs = with pkgs; [
    binutils
    graphviz
    texlive.combined.scheme-medium
    gnuplot
    sqlite
    jq
    xsv
  ];

  utilPkgs = with pkgs; [
    bitwarden
    gnome.geary
    qalculate-gtk
    anki
    zathura
    spotify
    spicetify-cli
    firefox-devedition-bin
  ]; # libreoffice & heimdall-gui

  chatPkgs = with pkgs; [ master.discord tdesktop element-desktop ];
  # signal-desktop & zoom-us

  mediaPkgs = with pkgs; [ gimp inkscape celluloid ];
  # obs-studio & blender

  gamingPkgs = with pkgs; [ lutris ];
  # osu-lazer

  winePkgs = with pkgs; [
    wineWowPackages.fonts
    wineWowPackages.staging
    (winetricks.override { wine = wineWowPackages.staging; })
  ];

in {
  home.packages = sysPkgs ++ gitPkgs ++ tuiPkgs ++ dictPkgs ++ nixPkgs
    ++ nixPkgs ++ devPkgs ++ lspPkgs ++ pyPkgs ++ emacsPkgs ++ utilPkgs
    ++ chatPkgs ++ mediaPkgs ++ gamingPkgs ++ winePkgs;
}
