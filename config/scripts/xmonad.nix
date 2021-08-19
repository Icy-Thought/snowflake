{ config, lib, pkgs, ... }: {

  imports = [
    ./brightness.nix
    ./screenshot.nix
    ./volume-control.nix
    ./micVol-control.nix
    # ./record-video.nix
  ];
}
