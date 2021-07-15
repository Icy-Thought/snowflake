{ config, lib, pkgs, ... }:

let
  commonPkgs = with pkgs; [
    wl-clipboard # Wayland c-p/c-v.
    zathura # Minimal Document Reader.
    latest.firefox-nightly-bin # Latest Firefox Browser.
    brave # Chromium-based browser.
    # Polychromatic # GUI to control OpenRazer.
    anki # Spaced repetition flashcard.
    celluloid # GTK frontend for MPV.
    gimp # Better Photoshop alternative.
    inkscape # Better Illustrator alternative.
    blender # 3D Creation/Animation.
    libreoffice # Better office alternative suit.
    obs-studio # Streaming/Recording.
    qalculate-gtk # Scientific calculator.
    discord # Latest Discord client.
    tdesktop # Telegram Desktop.
    signal-desktop # More secure WA alternative.
    element-desktop # Matrix client by Element.
    easytag # M3U Editor.
    transmission-gtk # BitTorrent Client.
    freetube # FOSS private YT app.
    glade # UI GTK designer.
    cawbird # GTK Twitter client.
    foliate # Minimal E-Book reader.
    zoom-us # Conferencing application..
    heimdall-gui # Suit to flash Android firmware.
    gsmartcontrol # HDD health inspection tool.
  ];

in { home.packages = commonPkgs; }
