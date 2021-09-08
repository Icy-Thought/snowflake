{ config, lib, pkgs, ... }:

let imports = [ ../../packages/sddm ];

in {
  inherit imports;

  services.xserver.displayManager.sddm.enable = true;
}
