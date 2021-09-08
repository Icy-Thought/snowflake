{ config, lib, pkgs, ... }:

let imports = [ ./leftwm ./dunst/leftwm.nix ./rofi ./polybar ./xresources ];

in {
  inherit imports;

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
