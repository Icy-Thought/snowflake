{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;

  colorclip = pkgs.writeScriptBin "colorclip" ''
    #!${pkgs.stdenv.shell}
    ICON = /tmp/colorclip.png

    xcolor -P 70 | xclip -selection clipboard
    COLOR = $(xclip -selection clipboard -out)

    convert -size 90x90 xc:"$COLOR" "$ICON"
    notify-send -u low -i "$ICON" 'xcolor' "$COLOR"
  '';

in { home.packages = [ colorclip ]; }
