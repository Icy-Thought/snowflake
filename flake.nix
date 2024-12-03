{
  description = "λ simple and configureable Nix-Flake repository!";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # System application(s)
    ragenix.url = "github:yaxitech/ragenix";
    kmonad = {
      url = "github:kmonad/kmonad?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Window Manager(s) + Extensions
    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-contrib.url =
      "github:icy-thought/xmonad-contrib"; # :TODO| replace with official after #582 == merged!
    pyprland.url = "github:hyprland-community/pyprland";

    # Application -> (Cached) Git
    spicetify-nix.url = "github:Gerg-L/spicetify-nix";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    rust.url = "github:oxalica/rust-overlay";

    # Submodules (temporary) # TODO
    emacs-dir = {
      url = "https://github.com/Icy-Thought/emacs.d.git";
      type = "git";
      submodules = true;
      flake = false;
    };
    nvim-dir = {
      url = "https://github.com/Icy-Thought/nvim.d.git";
      type = "git";
      submodules = true;
      flake = false;
    };
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
      pkgs = mkPkgs nixpkgs [ self.overlays.default ];
      pkgs-unstable = mkPkgs nixpkgs-unstable [ ];

      lib = nixpkgs.lib.extend (final: prev: {
        my = import ./lib {
          inherit pkgs inputs;
          lib = final;
        };
      });
    in {
      lib = lib.my;

      overlays = (mapModules ./overlays import) // {
        default = final: prev: {
          unstable = pkgs-unstable;
          my = self.packages.${system};
        };

        nvfetcher = final: prev: {
          sources = builtins.mapAttrs (_: p: p.src)
            ((import ./packages/_sources/generated.nix) {
              inherit (final) fetchurl fetchgit fetchFromGitHub dockerTools;
            });
        };
      };

      packages."${system}" = mapModules ./packages (p: pkgs.callPackage p { });

      nixosModules = {
        snowflake = import ./.;
      } // mapModulesRec ./modules import;

      nixosConfigurations = mapHosts ./hosts { };

      devShells."${system}".default = import ./shell.nix { inherit lib pkgs; };

      templates.full = {
        path = ./.;
        description = "λ well-tailored and configureable NixOS system!";
      } // import ./templates;

      templates.default = self.templates.full;

      # TODO: deployment + template tool.
      # apps."${system}" = {
      #   type = "app";
      #   program = ./bin/hagel;
      # };
    };
}
