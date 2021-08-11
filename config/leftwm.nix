{ config, lib, pkgs, ... }: {

  imports = [
    ./leftwm
    ./dunst/leftwm.nix
    ./picom/leftwm.nix
    ./rofi
    ./polybar
    ./xresources
  ];

  xdg.configFile."leftwm/themes/garden/liquid" = {
    source = ./liquid;
    recursive = true;
  };

  xdg.configFile."leftwm/themes/garden" = {
    source = ./scripts;
    recursive = true;
  };

  xdg.configFile."leftwm/themes/garden/assets" = {
    source = ./assets;
    recursive = true;
  };

}
