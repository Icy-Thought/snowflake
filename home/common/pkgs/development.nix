{ config, lib, pkgs, ... }:

let
  nixPkgs = [
    pkgs.nixfmt # Nix Code Formatter.
    pkgs.nixpkgs-fmt # [...] -> for nixpkgs.
    pkgs.nix-top # Tracks nix-builds.
    pkgs.nixpkgs-review # Review nixpkgs PR.
    pkgs.crate2nix # Nix-build File for Rust Crates.
    pkgs.cabal2nix # Cabal -> .nix Files.
  ];

  devPkgs = with pkgs; [
    pkgs.languagetool # Proof-Reading.
    # pkgs.gcc11 # GNU Compiler Collection.
    # pkgs.gnumake # Control Exec. Files.
    # pkgs.cmake # Auto Testing & Packaging.
    pkgs.rnix-lsp # Nix-lsp server.
    pkgs.rust-bin.nightly.latest.default # Latest Rust Compiler.
    pkgs.ghc # Glasgow Haskell Compiler.
    pkgs.cabal-install # Cabal & Hackage CLI.
    pkgs.sumneko-lua-language-server # Lua Language Server.
    # pkgs.nodejs-16_x # I/O Framwork for JS v8.
    pkgs.hugo # Modern Static Web Engine.
    pkgs.openssl # SSL & TLS Protocols.
  ];

  lspPkgs = [
    pkgs.ccls # C/C++ language Server - Clang.
    pkgs.rust-analyzer # Rust Completion.
    pkgs.haskell-language-server # LSP server for GHC
    # pkgs.nodePackages.pyright # Python.
    # pkgs.nodePackages.typescript-language-server # TypeScript.
    pkgs.nodePackages.bash-language-server # Bash.
  ];

  pyPkgs = [
    pkgs.python39 # Py-lang.
    pkgs.pipenv # Py-dev Workflow for Humans.
    pkgs.python39Packages.black # Python Code Formatter.
    pkgs.python39Packages.isort # Sort Py-imports.
    pkgs.python39Packages.pyflakes # Py-error Check.
    pkgs.python39Packages.nose-timer # Nosetests Timer.
    pkgs.python39Packages.nose-exclude # Exclude Dirs from nosetests.
    pkgs.python39Packages.pytest # Py-test Framework.
  ];

  emacsPkgs = [
    pkgs.graphviz # Graph Visualization.
    pkgs.tectonic # LaTeX Support.
    pkgs.gnuplot # Plotting Through Programming.
    pkgs.sqlite # Serverless SQL Database.
    pkgs.jq # Lightweight JSON Processor.
    pkgs.xsv # Fast CSV Toolkit (Rust).
  ];

in {
  home.packages = nixPkgs ++ devPkgs ++ lspPkgs ++ emacsPkgs;
} # Disable pyPkgs
