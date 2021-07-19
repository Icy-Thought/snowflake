{ config, lib, pkgs, ... }:

{
  imports = [ ./leftwm ./dunst ./rofi ./picom ./polybar ./xresources ];

  xdg.configFile."leftwm/themes/Garden" = {
    "skeleton" = {
      source = ./skeleton;
      recursive = true;
    };
  };
}
