{ config, lib, pkgs, ... }:

let
  devPkgs = with pkgs; [
    rust-bin.nightly.latest.default # Latest Rust compiler.
    rust-analyzer # Rust completion.
    sumneko-lua-language-server # Lua language server.
    gcc11 # GNU Compiler Collection.
    ccls # C/C++ language server - Clang.
    gnumake # Control exec. files.
    cmake # Auto Testing & packaging.
    nodejs-16_x # I/O framwork for JS v8.
    languagetool # Proofreading.
    openssl # SSL and TLS protocols.
    sqlite # Serverless SQL database.
    jq # Lightweight JSON processor.
    xsv # Fast CSV toolkit (Rust).
    python39 # Py-lang.
    pipenv # Py-dev workflow for humans.
    graphviz # Graph visualization.
    tectonic # LaTeX support.
    hugo # Modern static web-engine.
  ];

  lspPkgs = with pkgs.nodePackages; [
    # javascript-typescript-langserver                # JavaScript/TypeScript.
    pyright # Python.
    typescript-language-server # TypeScript.
    bash-language-server # Bash.
  ];

  pyPkgs = with pkgs.python39Packages; [
    black # Python code formatter.
    isort # Sort Py-imports.
    pyflakes # Py-error check.
    nose-timer # Nosetests timer.
    nose-exclude # Exclude dirs from nosetests.
    pytest # Py-test framework.
  ];

in { home.packages = devPkgs ++ lspPkgs ++ pyPkgs; }
