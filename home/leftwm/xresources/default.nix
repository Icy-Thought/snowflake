{ config, lib, pkgs, ... }: {

  xresources.properties = {
    "*.foreground" = "#000000";
    "*.background" = "#F8F8F2";
    "*.cursorColor" = "#F8F8F2";

    "*.color0" = "#3D4C5F";
    "*.color8" = "#56687E";

    # Red
    "*.color1" = "#EE4F84";
    "*.color9" = "#F48FB1";

    # Green
    "*.color2" = "#53E2AE";
    "*.color10" = "#A1EFD3";

    # Yellow
    "*.color3" = "#F1FF52";
    "*.color11" = "#F1FA8C";

    # Blue
    "*.color4" = "#6498EF";
    "*.color12" = "#92B6F4";

    # Magenta
    "*.color5" = "#985EFF";
    "*.color13" = "#BD99FF";

    # Cyan
    "*.color6" = "#24D1E7";
    "*.color14" = "#87DFEB";

    # White
    "*.color7" = "#E5E5E5";
    "*.color15" = "#F8F8F2";
  };

}
