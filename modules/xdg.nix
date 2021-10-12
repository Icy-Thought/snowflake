{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    gtkUsePortal = true;
  };
}
