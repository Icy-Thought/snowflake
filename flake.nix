{
  description = "λ well-tailored and configureable NixOS system!";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs.url = "github:nix-community/emacs-overlay";
    nvim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    rust.url = "github:oxalica/rust-overlay";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-unstable,
    ...
  }: let
    inherit (lib.my) mapModules mapModulesRec mapHosts;
    system = "x86_64-linux";

    mkPkgs = pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };
    pkgs = mkPkgs nixpkgs [self.overlays.default];
    pkgs' = mkPkgs nixpkgs-unstable [];

    lib = nixpkgs.lib.extend (final: prev: {
      my = import ./lib {
        inherit pkgs inputs;
        lib = final;
      };
    });
  in {
    lib = lib.my;

    overlays =
      (mapModules ./overlays import)
      // {
        default = final: prev: {
          unstable = pkgs';
          my = self.packages.${system};
        };
      };

    packages."${system}" = mapModules ./packages (p: pkgs.callPackage p {});

    nixosModules =
      {
        snowflake = import ./.;
      }
      // mapModulesRec ./modules import;

    nixosConfigurations = mapHosts ./hosts {};

    devShells."${system}".default = import ./shell.nix {inherit pkgs;};

    templates.full =
      {
        path = ./.;
        description = "λ well-tailored and configureable NixOS system!";
      }
      // import ./templates;

    templates.default = self.templates.full;

    # TODO: deployment + template tool.
    # apps."${system}" = {
    #   type = "app";
    #   program = ./bin/hagel;
    # };
  };
}
