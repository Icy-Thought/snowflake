{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  xdg.portal.enable = true;
  xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  xdg.portal.gtkUsePortal = true;
}
