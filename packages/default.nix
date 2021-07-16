{ config, pkgs, ... }: {

  imports = [
    # ./kernel.nix
    ./utility.nix
    ./environment.nix
  ];
  environment.shells = with pkgs; [ bash fish ];

}
