{ config, lib, pkgs, ... }: {

  xresources.properties = {
    "Xft.antialias" = 1;
    "Xft.hinting" = 1;
    "Xft.autohint" = 0;
    "Xft.hintstyle" = "hintslight";
    "Xft.rgba" = "rgb";
    "Xft.lcdfilter" = "lcddefault";

    "Xcursor.theme" = "bloom";
    "!Xcursor.size" = 16;

    "xiv.font" = "JetBrainsMonoMedium Nerd Font:style=Medium:size=9";

    "!Xmessage.font" = "FiraCode Nerd Font:pixelsize=14";

    "t.font" = "JetBrainsMonoMedium Nerd Font=size=10:weight=90";
    "t.borderpx" = 10;
    "t.opacity" = 255;
    "t.alpha" = "1.0";
    "t.cursorshape" = 1;
    "t.blinktimeout" = 900;

    "monad.font" = "xft:JetBrainsMonoMedium Nerd Font:size=10:style=Bold";
    "monad.font.gtk" = "JetBrainsMonoMedium Nerd Font, 10";

    # Aquarium - FrenzyExists
    "*.foreground" = "#E1E1E1";
    "*.background" = "#1B1B23";
    "*.cursorColor" = "#B8DCEB";

    # Black
    "*.color0" = "#1B1B23";
    "*.color8" = "#919BA0";

    # Red
    "*.color1" = "#BF616A";
    "*.color9" = "#BF616A";

    # Green
    "*.color2" = "#A3BE8C";
    "*.color10" = "#A3BE8C";

    # Yellow
    "*.color3" = "#EBCB8B";
    "*.color11" = "#EBCB8B";

    # Blue
    "*.color4" = "#8fa1b3";
    "*.color12" = "#8fa1b3";

    # Magenta
    "*.color5" = "#b48ead";
    "*.color13" = "#b48ead";

    # Cyan
    "*.color6" = "#96b5b4";
    "*.color14" = "#96b5b4";

    # White
    "*.color7" = "#e1e1e1";
    "*.color15" = "#c0c5ce";
  };

}
