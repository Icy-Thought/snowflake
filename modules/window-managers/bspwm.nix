{ config, lib, pkgs, ... }:

{
  services.xersver = {
    xkbOptions = "caps:ctrl_modifier";
    windowManager.bspwm = {
      enable = true;
      # configFile = ;
      # sxhkd.configFile = ;
    };
    displayManager.defaultSession = "none+bspwm";
  };
}
