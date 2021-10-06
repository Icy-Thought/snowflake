{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  fonts.enableDefaultFonts = true;

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "JetBrainsMono" "FantasqueSansMono" ]; })
    iosevka
    comfortaa
    source-code-pro
    emacs-all-the-icons-fonts
    liberation_ttf
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
  ];

  fonts.fontconfig.defaultFonts = {
    serif = [ "Cantarell" "Noto Kufi Arabic" ];
    sansSerif = [ "Cantarell" "Noto Kufi Arabic" ];
    monospace = [ "Cantarell" "Noto Kufi Arabic" ];
  };
}
