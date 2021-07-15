{ config, lib, pkgs, ... }:

let
  utilPkgs = with pkgs; [
    binutils # Manipulating binaries.
    moreutils # Extension to GNU utils.
    killall # Completely eradicate processes.
    direnv # Shell extension to manage env.
    pciutils # lspci, setpci & update-pciids.
    psmisc # Useful utilities.
    wireguard # Wireguard tools.
    gnupg # Encrypt/Decrypt.
    firejail # Namespace-based sandboxing.
    exiftool # Control file metadata.
    fail2ban # Scans failed login attempts.
    usbguard # Protect against infected USB.
    xclip # Copy/Paste in xterm.
    wl-clipboard # Wayland c-p/c-v.
    gh # Official GH client.
    pv # Progress-bar.
    fd # faster find.
    exa # better ls.
    ripgrep # faster grep.
    skim # faster fzf.
    tree # Tree view.
    bustle # Sequence diagrams (dbus).
    dfeet # dbus debugger.
    diffoscope # In-depth comparison tool.
    common-updater-scripts # Common nix updating scripts????
    zstd # Undo fu-session/tree-compression.
    unrar # File uncompressor.
    agenix # age-encrypted secrets.
  ];

in { environment.systemPackages = utilPkgs; }
