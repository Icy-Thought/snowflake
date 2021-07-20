{ config, lib, pkgs, ... }:

{
  imports = [ ./leftwm ./dunst ./rofi ./picom ./polybar ./xresources ];

  xdg.configFile."leftwm/themes/garden" = {
    source = ./skeleton;
    recursive = true;
  };
}
