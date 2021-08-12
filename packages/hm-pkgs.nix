{ config, lib, pkgs, ... }:

let
  sysPkgs = [
    # pkgs.polychromatic # GUI to control OpenRazer.
    pkgs.transmission-gtk # BitTorrent Client.
    # pkgs.uutils-coreutils # Rust GNU-coreutils Alt.
    pkgs.coreutils # GNU-coreutils.
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

  gitPkgs = [
    pkgs.gitAndTools.diff-so-fancy # Colored git diff.
    pkgs.gitAndTools.git-crypt # git File Encryption.
    pkgs.gitAndTools.tig # diff & commit View.
  ];

  tuiPkgs = [
    pkgs.nnn # TUI File Manager.
    pkgs.glances # Curses-based Monitoring Tool.
    pkgs.neofetch # Fetch System Information.
    pkgs.youtube-dl # YouTube Media Downloader.
    # pkgs.spotify-tui # TUI for Premium Spotify Users.
    # pkgs.speedtest-cli # TUI Speedtest.
  ];

  dictPkgs = [
    pkgs.aspell # Spelling Support.
    pkgs.aspellDicts.en # en_US Aspell.
    pkgs.aspellDicts.sv # sv_SE Aspell.
    pkgs.hunspellDicts.sv_SE # sv_SE Hunspell.
    pkgs.hunspellDicts.en_US # en_US Hunspell.
  ];

  nixPkgs = [
    pkgs.nixfmt # Nix Code Formatter.
    pkgs.nixpkgs-fmt # [...] -> for nixpkgs.
    pkgs.nix-top # Tracks nix-builds.
    pkgs.nixpkgs-review # Review nixpkgs PR.
    pkgs.crate2nix # Nix-build File for Rust Crates.
    pkgs.cabal2nix # Cabal -> .nix Files.
    pkgs.any-nix-shell # Fish/ZSH Support.
    pkgs.nix-direnv # Fast nix-impl of direnv.
  ];

  devPkgs = with pkgs; [
    pkgs.languagetool # Proof-Reading.
    # pkgs.gcc11 # GNU Compiler Collection.
    # pkgs.gnumake # Control Exec. Files.
    # pkgs.cmake # Auto Testing & Packaging.
    pkgs.rust-bin.nightly.latest.default # Latest Rust Compiler.
    pkgs.ghc # Glasgow Haskell Compiler.
    pkgs.cabal-install # Cabal & Hackage CLI.
    pkgs.stylish-haskell # HS code prettifier.
    pkgs.sumneko-lua-language-server # Lua Language Server.
    # pkgs.nodejs-16_x # I/O Framwork for JS v8.
    pkgs.hugo # Modern Static Web Engine.
    pkgs.openssl # SSL & TLS Protocols.
  ];

  lspPkgs = [
    pkgs.ccls # C/C++ language Server - Clang.
    pkgs.rust-analyzer # Rust Completion.
    pkgs.rnix-lsp # Nix-lsp server.
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

  utilPkgs = [
    pkgs.gnome.geary # GTK Mail Client.
    # pkgs.libreoffice # Better Office Alternative Suit.
    pkgs.latest.firefox-nightly-bin # Latest Firefox Browser.
    pkgs.ueberzug # Display Images in CLI.
    pkgs.anki # Spaced Repetition Flashcard.
    pkgs.zathura # Minimal Document Reader.
    pkgs.foliate # Minimal E-Book Reader.
    # pkgs.heimdall-gui # Suit to Flash Android Firmware.
    pkgs.qalculate-gtk # Scientific Calculator.
  ];

  chatPkgs = [
    # pkgs.zoom-us # Conferencing Application..
    pkgs.discord # Latest Discord Client.
    pkgs.tdesktop # Telegram Desktop.
    # pkgs.signal-desktop # More Secure WA Alternative.
    pkgs.element-desktop # Matrix Client by Element.
  ];

  mediaPkgs = [
    # pkgs.obs-studio # Streaming/Recording.
    # pkgs.blender # 3D Creation/Animation.
    pkgs.celluloid # GTK Frontend for MPV.
    # pkgs.freetube # FOSS Private YT App.
    pkgs.gimp # Better Photoshop Alternative.
    pkgs.inkscape # Better Illustrator Alternative.
    pkgs.kid3 # Audio Tag Editor.
  ];

  gamingPkgs = [
    pkgs.lutris # WINE Gaming Platform.
    # pkgs.osu-lazer # FOSS Rythm Game!
  ];

  winePkgs = [
    pkgs.winePackages.staging # Wine-Staging.
    pkgs.winePackages.fonts # MS-fonts by wine-project.
  ];

in {
  home.packages = sysPkgs ++ gitPkgs ++ tuiPkgs ++ dictPkgs ++ nixPkgs
    ++ nixPkgs ++ devPkgs ++ lspPkgs ++ emacsPkgs ++ utilPkgs ++ chatPkgs
    ++ mediaPkgs ++ gamingPkgs ++ winePkgs;
} # Disabled pyPkgs + kernelPkgs
