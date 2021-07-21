{ config, lib, pkgs, ... }:

{
  imports = [
    # ./xmonad/config
    ./leftwm/config
    # ./bspwm/config
  ];
}
