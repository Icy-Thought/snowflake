{ config, lib, pkgs, ... }:

let imports = [ ../desktop/fcitx5.nix ../../packages/sddm.nix ];

in {
  inherit imports;

  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.sddm.enable = true;
}
