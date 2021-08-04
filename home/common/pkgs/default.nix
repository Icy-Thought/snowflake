{ config, lib, pkgs, ... }: {

  imports = [
    ./common.nix
    ./environment.nix
    ./development.nix
    ./utility.nix
    ./gaming.nix
  ];
}
