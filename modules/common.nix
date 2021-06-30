{ inputs, config, lib, pkgs, ... }:
{
  imports = [ 
    ./primary.nix
    ./nixpkgs.nix
    ./packages.nix
    ./wireguard
  ];

  hm = import ./home-manager;

  environment.etc = {
    home-manager.source = "${inputs.home-manager}";
    nixpkgs.source = "${inputs.nixpkgs}";
    # "nixos".source = "${config.home.homeDirectory}/git/Snowflake";
  };

  user = {
    description = "Icy-Thought";
    home = "/home/${config.user.name}";
    shell = pkgs.fish;
  };

  home-manager = {
    extraSpecialArgs = { inherit inputs lib; };
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  age.secrets = {
    "wg-akkad/privateKey" = {
      file = ../secrets/wg-akkad/privateKey.age;
      owner = "${config.user.name}";
      mode = "0440";
    };
  };

}
