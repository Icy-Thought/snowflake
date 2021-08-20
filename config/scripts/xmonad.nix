{ config, lib, pkgs, ... }: {

  imports = [
    ./brightness.nix
    ./screenshot.nix
    ./volume.nix
    ./microphone.nix
    # ./screen-recording.nix
  ];
}
