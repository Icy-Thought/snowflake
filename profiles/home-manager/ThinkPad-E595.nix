{ config, pkgs, ... }: {

  imports = [
    ../../modules/desktop-managers/hm.nix
    ../../modules/window-managers/hm.nix
  ];
}
