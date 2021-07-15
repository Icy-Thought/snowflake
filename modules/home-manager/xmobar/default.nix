{ config, lib, pkgs, ... }:

{
  programs.xmobar = {
    enable = true;
    extraConfig = builtins.readFile ./xmobar.hs;
  };
}
