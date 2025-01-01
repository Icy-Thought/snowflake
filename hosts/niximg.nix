{ config, lib, pkgs, modulesPath, ... }:

with lib; {
  imports = [ "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix" ];

  # In case of proprietary wireless drivers
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "wl" ];
    extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
  };

  environment.systemPackages = with pkgs; [ zsh git nixStable ];
}
# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=./default.nix
