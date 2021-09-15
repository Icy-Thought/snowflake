{ config, lib, pkgs, ... }:

let imports = [ ../display-managers/sddm.nix ];

in {
  inherit imports;

  services.xserver.desktopManager.plasma5.enable = true;

}
