{ config, lib, pkgs, ... }:

let
  sysPkgs = with pkgs; [
    # Polychromatic # GUI to control OpenRazer.
    wl-clipboard # Wayland c-p/c-v.
    gsmartcontrol # HDD health inspection tool.
    transmission-gtk # BitTorrent Client.
  ];

  utilPkgs = with pkgs; [
    glade # UI GTK designer.
    anki # Spaced repetition flashcard.
    latest.firefox-nightly-bin # Latest Firefox Browser.
    libreoffice # Better office alternative suit.
    zathura # Minimal Document Reader.
    foliate # Minimal E-Book reader.
    heimdall-gui # Suit to flash Android firmware.
    qalculate-gtk # Scientific calculator.
  ];

  chatPkgs = with pkgs; [
    zoom-us # Conferencing application..
    discord # Latest Discord client.
    tdesktop # Telegram Desktop.
    signal-desktop # More secure WA alternative.
    element-desktop # Matrix client by Element.
  ];

  mediaPkgs = with pkgs; [
    obs-studio # Streaming/Recording.
    celluloid # GTK frontend for MPV.
    freetube # FOSS private YT app.
    blender # 3D Creation/Animation.
    gimp # Better Photoshop alternative.
    inkscape # Better Illustrator alternative.
    easytag # M3U Editor.
  ];

in { home.packages = sysPkgs ++ chatPkgs ++ mediaPkgs ++ utilPkgs; }
