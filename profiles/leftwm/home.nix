{ config, lib, pkgs, ... }:

let imports = [ ./leftwm ./dunst ./rofi ./polybar ./xresources ];

in { inherit imports; }
