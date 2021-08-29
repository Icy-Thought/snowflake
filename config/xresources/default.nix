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

    # Ayu-dark -> dempfi
    "*.foreground" = "#B3B1AD";
    "*.background" = "#0A0E14";
    "*.cursorColor" = "#E6B450";

    # Black
    "*.color0" = "#0A0E14";
    "*.color8" = "#3D424D";

    # Red
    "*.color1" = "#F07178";
    "*.color9" = "#D96C75";

    # Green
    "*.color2" = "#91B362";
    "*.color10" = "#C2D94C";

    # Yellow
    "*.color3" = "#E6B450";
    "*.color11" = "#FFB454";

    # Blue
    "*.color4" = "#39BAE6";
    "*.color12" = "#59C2FF";

    # Magenta
    "*.color5" = "#F29668";
    "*.color13" = "#FF8F40";

    # Cyan
    "*.color6" = "#6994BF";
    "*.color14" = "#95E6CB";

    # White
    "*.color7" = "#626A73";
    "*.color15" = "#B3B1AD";
  };

}
