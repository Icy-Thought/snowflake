{ config, lib, pkgs, ... }:

let
  nixPkgs = with pkgs; [
    nixfmt # Nix Code Formatter.
    nixpkgs-fmt # [...] -> for nixpkgs.
    nix-prefetch-github # Prefetch from GH.
    nixpkgs-review # Review nixpkgs PR.
    nix-top # Tracks nix-builds.
    lorri # Project's nix-env.
    hydra-check # Hydra Build Status Check.
  ];

  devPkgs = with pkgs; [
    languagetool # Proof-Reading.
    sumneko-lua-language-server # Lua Language Server.
    nodejs-16_x # I/O Framwork for JS v8.
    hugo # Modern Static Web Engine.
    openssl # SSL & TLS Protocols.
  ];

  lspPkgs = with pkgs.nodePackages; [
    # javascript-typescript-langserver                # JavaScript/TypeScript.
    pyright # Python.
    typescript-language-server # TypeScript.
    bash-language-server # Bash.
  ];

  ccppPkgs = with pkgs; [
    gcc11 # GNU Compiler Collection.
    ccls # C/C++ language Server - Clang.
    gnumake # Control Exec. Files.
    cmake # Auto Testing & Packaging.
  ];

  rsPkgs = with pkgs; [
    rust-bin.nightly.latest.default # Latest Rust Compiler.
    rust-analyzer # Rust Completion.
    crate2nix # Nix-build File for Rust Crates.
  ];

  hsPkgs = with pkgs;
    [
      # ghc # Glasgow Haskell Compiler.
      # cabal2nix # Cabal -> .nix Files.
    ];

  pyPkgs = with pkgs.python39Packages; [
    black # Python Code Formatter.
    isort # Sort Py-imports.
    pyflakes # Py-error Check.
    nose-timer # Nosetests Timer.
    nose-exclude # Exclude Dirs from nosetests.
    pytest # Py-test Framework.
  ];

  pyExtPkgs = with pkgs; [
    python39 # Py-lang.
    pipenv # Py-dev Workflow for Humans.
  ];

  emacsPkgs = with pkgs; [
    graphviz # Graph Visualization.
    tectonic # LaTeX Support.
    sqlite # Serverless SQL Database.
    jq # Lightweight JSON Processor.
    xsv # Fast CSV Toolkit (Rust).
  ];

in {
  home.packages = nixPkgs ++ devPkgs ++ lspPkgs ++ ccppPkgs ++ rsPkgs ++ hsPkgs
    ++ pyPkgs ++ pyExtPkgs ++ emacsPkgs;
}
