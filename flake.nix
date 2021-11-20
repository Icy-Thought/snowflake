{
  description = "λ well-tailored and configureable NixOS system!";

  inputs = {
    # Core Dependencies:
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    # XMonad:
    # (remove) xmonad & contrib after "ConditionalLayoutModifier" merger
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:icy-thought/xmonad-contrib";
    taffybar.url = "github:taffybar/taffybar";

    # Extras:
    emacs.url = "github:nix-community/emacs-overlay";
    rust.url = "github:oxalica/rust-overlay";

    picom.url = "github:yshui/picom/next";
    picom.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, ... }:

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
      pkgs' = mkPkgs nixpkgs-unstable [ ];

      lib = nixpkgs.lib.extend (self: super: {
        my = import ./lib {
          inherit pkgs inputs;
          lib = self;
        };
      });

    in {
      lib = lib.my;

      overlay = final: prev: {
        unstable = pkgs';
        my = self.packages."${system}";
        picom = prev.picom.overrideAttrs (_: { src = inputs.picom; });
      };

      overlays = mapModules ./overlays import;

      packages."${system}" = mapModules ./packages (p: pkgs.callPackage p { });

      nixosModules = {
        snowflake = import ./.;
      } // mapModulesRec ./modules import;

      nixosConfigurations = mapHosts ./hosts { };

      devShell."${system}" = import ./shell.nix { inherit pkgs; };

      # TODO: new struct.
      templates.full = {
        path = ./.;
        description = "λ well-tailored and configureable NixOS system!";
      };

      template.minimal = {
        path = ./templates/minimal;
        description = "λ well-tailored and configureable NixOS system!";
      };

      defaultTemplate = self.templates.minimal;

      # TODO: deployment + template tool.
      # defaultApp."${system}" = {
      #   type = "app";
      #   program = ./bin/hagel;
      # };

    };
}
