{ config, lib, pkgs, ... }: {

  imports = [
    ./colorclip.nix
    ./date.nix
    ./lockctl.nix
    ./msclip.nix
    ./playerctl.nix
    ./pulsepipe.nix
    ./shotclip.nix
    ./ssclip.nix
    ./tray-padding-icon.nix
    ./volctl.nix
    ./volume.nix
    ./weather.nix
  ];
}
