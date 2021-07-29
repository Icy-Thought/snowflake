{ config, lib, pkgs, ... }: {

  imports = [ ../pkgs/sddm ];

  services.xserver.displayManager.sddm.enable = true;
}
