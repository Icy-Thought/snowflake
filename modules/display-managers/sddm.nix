{ config, lib, pkgs, ... }: {

  imports = [ ../../packages/sddm ];

  services.xserver.displayManager.sddm.enable = true;
}
