{ config, lib, pkgs, ... }:

{
  imports = [ ./leftwm ./dunst ./rofi ./polybar ./xresources ];

  services.picom.package = pkgs.picom-jonaburg;

  xdg.configFile."leftwm/themes/garden" = {
    source = ./skeleton;
    recursive = true;
  };
}
