{ config, lib, pkgs, ... }: {

  services.stalonetray = {
    enable = true;
    config = {
      geometry = "1x1-17+5";
      max-geometry = "10x1-17+5";
      transparent = true;
      tint-color = "#2c313a";
      tint-level = 255;
      grow-gravity = "NE";
      icon-gravity = "NW";
      icon-size = 20;
      sticky = true;
      window-type = "dock";
      window-strut = "top";
      skip-taskbar = true;
    };
  };

}
