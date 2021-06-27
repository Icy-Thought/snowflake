{ inputs, config, lib, pkgs, ... }:
{
  imports = [ 
      ./primary.nix
      ./nixpkgs.nix
      ./wireguard.nix
    ];

    hm = import ./home-manager;

  environment.etc = {
    home-manager.source = "${inputs.home-manager}";
    nixpkgs.source = "${inputs.nixpkgs}";
  };

  user = {
    description = "icy-thought";
    home = "/home/${config.user.name}";
    shell = pkgs.fish;
  };

  users = { # Don't forget to set password through `passwd`!
    defaultUserShell = pkgs.fish;
    mutableUsers = false;
    
    users = {
      "${config.user.name}" = {
        isNormalUser = true;
        createHome = true;
        useDefaultShell = true;
        extraGroups = [ "wheel" "network" "plugdev" "adbusers" ];
        hashedPassword = "$6$DMQjZ0Nn8JAb$2MBYjRZvhACwUJrDXI6GciNglr.KM3Yaza4CMUaG8HCxOJ2EtRqZZKvTBzRhIPQWjKiYeU3cCpntQNkToiUeu0";
      };
    };
  };

  home-manager = {
    extraSpecialArgs = { inherit inputs lib; };
    useGlobalPkgs = true;
    useUserPackages = true;
  };

}
