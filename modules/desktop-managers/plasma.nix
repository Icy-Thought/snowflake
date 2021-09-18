{ config, lib, pkgs, ... }:

let imports = [ ../nixos/fcitx5.nix ../display-managers/sddm.nix ];

in {
  inherit imports;

  services.xserver.desktopManager.plasma5.enable = true;

}
