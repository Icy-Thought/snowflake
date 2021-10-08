{
  description = "λ well-tailored and configureable NixOS system!";

  inputs = {
    # Core Dependencies
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Extras
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:icy-thought/xmonad-contrib";
    taffybar.url = "github:taffybar/taffybar";

    picom-jonaburg = {
      url = "github:jonaburg/picom";
      flake = false;
    };

    rust.url = "github:oxalica/rust-overlay";
    emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-master, ... }:

    let
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays ++ (lib.attrValues self.overlays);
        };

      pkgs = mkPkgs nixpkgs [ self.overlay ];
      pkgs' = mkPkgs nixpkgs-master [ ];

      lib = nixpkgs.lib.extend (self: super: {
        my = import ./lib {
          inherit pkgs inputs;
          lib = self;
        };
      });

    in {
      lib = lib.my;

      overlay = final: prev: {
        master = pkgs';
        my = self.packages."${system}";
        picom = prev.picom.overrideAttrs (_: { src = inputs.picom-jonaburg; });
      };

      overlays = mapModules ./overlays import;

      packages."${system}" = mapModules ./packages (p: pkgs.callPackage p { });

      nixosModules = {
        snowflake = import ./.;
      } // mapModulesRec ./modules import;

      nixosConfigurations = mapHosts ./hosts { };

      devShell."${system}" = import ./shell.nix { inherit pkgs; };

      # templates = {
      #   full = {
      #     path = ./.;
      #     description = "A grossly incandescent nixos config";
      #   };
      #   minimal = {
      #     path = ./templates/minimal;
      #     description = "A grossly incandescent and minimal nixos config";
      #   };
      # };
      # defaultTemplate = self.templates.minimal;
      #
      # defaultApp."${system}" = {
      #   type = "app";
      #   program = ./bin/bruh;
      # };

    };
}
