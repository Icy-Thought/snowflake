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

    "xiv.font" = "JetBrains Mono:style=Medium:size=9";

    "!Xmessage.font" = "FiraCode Nerd Font:pixelsize=14";

    "t.font" = "JetBrainsMono Nerd Font =size=10:weight=90";
    "t.borderpx" = 10;
    "t.opacity" = 255;
    "t.alpha" = "1.0";
    "t.cursorshape" = 1;
    "t.blinktimeout" = 900;

    "monad.font" = "xft:JetBrains Mono:size=10:style=Bold";
    "monad.font.gtk" = "JetBrains Mono Medium, 10";

    "*.foreground" = "#c8ccd4";
    "*.background" = "#1e2127";
    "*.cursorColor" = "#abb2bf";

    "*.color0" = "#2c313a";
    "*.color8" = "#545862";

    # Red
    "*.color1" = "#ff6c6b";
    "*.color9" = "#ff8b92";

    # Green
    "*.color2" = "#98be65";
    "*.color10" = "#ddffa7";

    # Yellow
    "*.color3" = "#ffcb6b";
    "*.color11" = "#ffe585";

    # Blue
    "*.color4" = "#51aeed";
    "*.color12" = "#9cc4ff";

    # Magenta
    "*.color5" = "#b58fae";
    "*.color13" = "#e1acff";

    # Cyan
    "*.color6" = "#56b6c2";
    "*.color14" = "#a3f7ff";

    # White
    "*.color7" = "#a6aebf";
    "*.color15" = "#ffffff";
  };

}
