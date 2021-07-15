{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./environment.nix
    ./development.nix
    ./editor.nix
    ./utility.nix
    ./gaming.nix
  ];
}
