{ config, lib, pkgs, ... }:

{
  imports = [ ../display-managers/sddm.nix ];

  services.xserver.desktopManager.plasma5.enable = true;
}
