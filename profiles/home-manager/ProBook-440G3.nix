{ config, pkgs, ... }:
{
  home-manager.packages = with pkgs; [
    kdenlive
  ];
}
