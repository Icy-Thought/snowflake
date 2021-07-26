{ config, lib, pkgs, ... }: {

  imports = [
    ./colorclip.nix
    ./date.nix
    ./default.nix
    ./lockctl.nix
    ./msclip.nix
    ./playerctl.nix
    ./pulsepipe.nix
    ./shotclip.nix
    ./ssclip.nix
    ./tray-padding-icon.nix
    ./volctl.nix
    ./volume.sh
    ./wall.nix
    ./weather.nix
  ];
}
