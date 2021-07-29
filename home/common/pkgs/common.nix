{ config, lib, pkgs, ... }:

let
  sysPkgs = with pkgs; [
    # Polychromatic # GUI to control OpenRazer.
    gsmartcontrol # HDD Health Inspection Tool.
    transmission-gtk # BitTorrent Client.
  ];

  utilPkgs = with pkgs; [
    ueberzug # Display Images in CLI.
    glade # UI GTK Designer.
    anki # Spaced Repetition Flashcard.
    latest.firefox-nightly-bin # Latest Firefox Browser.
    libreoffice # Better Office Alternative Suit.
    zathura # Minimal Document Reader.
    foliate # Minimal E-Book Reader.
    heimdall-gui # Suit to Flash Android Firmware.
    qalculate-gtk # Scientific Calculator.
  ];

  chatPkgs = with pkgs; [
    zoom-us # Conferencing Application..
    discord # Latest Discord Client.
    tdesktop # Telegram Desktop.
    signal-desktop # More Secure WA Alternative.
    element-desktop # Matrix Client by Element.
  ];

  mediaPkgs = with pkgs; [
    obs-studio # Streaming/Recording.
    celluloid # GTK Frontend for MPV.
    freetube # FOSS Private YT App.
    blender # 3D Creation/Animation.
    gimp # Better Photoshop Alternative.
    inkscape # Better Illustrator Alternative.
    kid3 # Audio Tag Editor.
  ];

in { home.packages = sysPkgs ++ chatPkgs ++ mediaPkgs ++ utilPkgs; }
