{ config, lib, pkgs, ... }: {

  xresources.properties = {
    # Ayu-dark -> dempfi
    "*.foreground" = "#E6E1CF";
    "*.background" = "#0F1419";
    "*.cursorColor" = "#F29718";

    # Black
    "*.color0" = "#1D242C";
    "*.color8" = "#686868";

    # Red
    "*.color1" = "#FF7733";
    "*.color9" = "#F07178";

    # Green
    "*.color2" = "#B8CC52";
    "*.color10" = "#CBE645";

    # Yellow
    "*.color3" = "#FFB454";
    "*.color11" = "#FFEE99";

    # Blue
    "*.color4" = "#36A3D9";
    "*.color12" = "#6871FF";

    # Magenta
    "*.color5" = "#CA30C7";
    "*.color13" = "#FF77FF";

    # Cyan
    "*.color6" = "#95E6CB";
    "*.color14" = "#A6FDE1";

    # White
    "*.color7" = "#C7C7C7";
    "*.color15" = "#FFFFFF";

  };

}
