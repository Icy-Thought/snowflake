{ config, lib, pkgs, ... }:

let
  sysPkgs = with pkgs; [
    tree # Tree view.
    wireguard # Wireguard Tools.
    binutils # Manipulating Binaries.
    moreutils # Extension to GNU Utils.
    pciutils # lspci, setpci & update-pciids.
    psmisc # Useful Utilities.
    killall # Completely Eradicate Processes.
    xclip # Copy/Paste in Xterm.
    wl-clipboard # Wayland c-p/c-v.
  ];

  altPkgs = with pkgs; [
    exa # Better ls.
    pv # Progress-bar.
    fd # Faster Find.
    ripgrep # Faster grep.
    skim # Faster fzf.
  ];

  utilPkgs = with pkgs; [
    direnv # Shell Extension to Manage env.
    gnupg # Encrypt/Decrypt.
    firejail # Namespace-based Sandboxing.
    exiftool # Control File Metadata.
    fail2ban # Scans Failed Login Attempts.
    usbguard # Protect Against Infected USB.
    gh # Official GH Client.
    bustle # Sequence Diagrams (dbus).
    dfeet # dbus Debugger.
    diffoscope # In-depth Comparison Tool.
    zstd # Undo fu-Session/tree-Compression.
    unrar # File Uncompressor.
    agenix # age-Encrypted Secrets.
  ];

in { environment.systemPackages = utilPkgs ++ sysPkgs ++ altPkgs; }
