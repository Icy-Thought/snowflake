{ inputs, config, lib, pkgs, ... }: {

  hm = import ./home-manager;

  imports = [ ./primary.nix ./nixpkgs.nix ../packages ./wireguard ];

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

}
