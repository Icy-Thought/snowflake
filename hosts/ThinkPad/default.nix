{ config, agenix, home, inputs, nixpkgs, overlays, ... }:


nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    agenix.nixosModules.age
    home.nixosModules.home-manager
    nixpkgs.nixosModules.notDetected

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      users.sirius = import ../../users/sirius;
    };

    nix = import ../../config/nix-conf.nix { inherit inputs system nixpkgs; };
      nixpkgs = { inherit config overlays; };
    }

    ./configuration.nix

  ];

  specialArgs = { inherit inputs; };
}
