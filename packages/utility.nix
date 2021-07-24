{ config, lib, pkgs, ... }:

let
  sysPkgs = with pkgs; [
    tree # Tree view.
    wireguard # Wireguard tools.
    binutils # Manipulating binaries.
    moreutils # Extension to GNU utils.
    pciutils # lspci, setpci & update-pciids.
    psmisc # Useful utilities.
    killall # Completely eradicate processes.
    xclip # Copy/Paste in xterm.
    wl-clipboard # Wayland c-p/c-v.
  ];

  altPkgs = with pkgs; [
    exa # better ls.
    pv # Progress-bar.
    fd # faster find.
    ripgrep # faster grep.
    skim # faster fzf.
  ];

  utilPkgs = with pkgs; [
    direnv # Shell extension to manage env.
    gnupg # Encrypt/Decrypt.
    firejail # Namespace-based sandboxing.
    exiftool # Control file metadata.
    fail2ban # Scans failed login attempts.
    usbguard # Protect against infected USB.
    gh # Official GH client.
    bustle # Sequence diagrams (dbus).
    dfeet # dbus debugger.
    diffoscope # In-depth comparison tool.
    common-updater-scripts # Common nix updating scripts????
    zstd # Undo fu-session/tree-compression.
    unrar # File uncompressor.
    agenix # age-encrypted secrets.
  ];

in { environment.systemPackages = utilPkgs ++ sysPkgs ++ altPkgs; }
