{ config, lib, pkgs, ... }:

let
  sysPkgs = [
    # pkgs.polychromatic # GUI to control OpenRazer.
    pkgs.transmission-gtk # BitTorrent Client.
  ];

  utilPkgs = [
    pkgs.ueberzug # Display Images in CLI.
    pkgs.anki # Spaced Repetition Flashcard.
    pkgs.latest.firefox-nightly-bin # Latest Firefox Browser.
    # pkgs.libreoffice # Better Office Alternative Suit.
    pkgs.zathura # Minimal Document Reader.
    pkgs.foliate # Minimal E-Book Reader.
    pkgs.heimdall-gui # Suit to Flash Android Firmware.
    pkgs.qalculate-gtk # Scientific Calculator.
  ];

  chatPkgs = [
    pkgs.zoom-us # Conferencing Application..
    pkgs.discord # Latest Discord Client.
    pkgs.tdesktop # Telegram Desktop.
    pkgs.signal-desktop # More Secure WA Alternative.
    pkgs.element-desktop # Matrix Client by Element.
  ];

  mediaPkgs = [
    pkgs.obs-studio # Streaming/Recording.
    # pkgs.blender # 3D Creation/Animation.
    pkgs.celluloid # GTK Frontend for MPV.
    pkgs.freetube # FOSS Private YT App.
    pkgs.gimp # Better Photoshop Alternative.
    pkgs.inkscape # Better Illustrator Alternative.
    pkgs.kid3 # Audio Tag Editor.
  ];

in { home.packages = sysPkgs ++ chatPkgs ++ mediaPkgs ++ utilPkgs; }
