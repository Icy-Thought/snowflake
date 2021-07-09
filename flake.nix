{
  description = "A structured and configureable NixOS system!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";

    xmonadGH = {
      url = "github:xmonad/xmonad";
      flake = false;
    };

    xmonad-extrasGH = {
      url = "github:xmonad/xmonad-extras";
      flake = false;
    };

    xmonad-contribGH = {
      url = "github:xmonad/xmonad-contrib";
      flake = false;
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    rust-overlay.url = "github:oxalica/rust-overlay";

    nixpkgs-mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };

    agenix.url = "github:ryantm/agenix";
    # naersk.url = "github:nmattia/naersk";

  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, home-manager
    , nixos-hardware, flake-utils, xmonadGH, xmonad-extrasGH, xmonad-contribGH
    , emacs-overlay, rust-overlay, nixpkgs-mozilla, agenix, ... }:

    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };

      overlays = with inputs; [
        agenix.overlay
        rust-overlay.overlay
        emacs-overlay.overlay
        (import nixpkgs-mozilla)

        (final: prev: rec {
          haskellPackages = prev.haskellPackages.override {
            overrides = self: super: rec {

              xmonad = let hpkg = self.callCabal2nix "xmonad" xmonadGH { };
              in pkgs.haskell.lib.dontCheck hpkg;

              xmonad-extras = let
                hpkg = self.callCabal2nix "xmonad-extras" xmonad-extrasGH { };
              in pkgs.haskell.lib.dontCheck hpkg;

              xmonad-contrib = let
                hpkg = self.callCabal2nix "xmonad-contrib" xmonad-contribGH { };
              in pkgs.haskell.lib.dontCheck hpkg;
            };
          };
        })

      ];
      # ++ map (name: import (./overlays + "/${name}"))
      # (builtins.attrNames (builtins.readDir ./overlays));

      lib = nixpkgs.lib.extend (final: prev: home-manager.lib);

      inherit (nixpkgs.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      inherit (builtins) listToAttrs map;

      # Generate default NixOS config
      mkNixosConfig = { system ? "x86_64-linux", hardwareModules
        , baseModules ? [
          agenix.nixosModules.age
          home-manager.nixosModules.home-manager
          ./modules/nixos
        ], extraModules ? [ ] }:

        nixosSystem {
          inherit system;
          modules = baseModules ++ hardwareModules ++ extraModules
            ++ [{ nixpkgs.overlays = overlays; }];
          specialArgs = { inherit inputs lib; };
        };

      # Generate default Home-Manager conf
      mkHomeConfig = { username, system ? "x86_64-linux"
        , baseModules ? [ ./modules/home-manager ], extraModules ? [ ] }:

        homeManagerConfiguration rec {
          inherit system username;
          homeDirectory = "/home/${username}";
          extraSpecialArgs = { inherit inputs lib; };
          configuration = {
            imports = baseModules ++ extraModules
              ++ [{ nixpkgs.overlays = overlays; }];
          };
        };

    in {
      nixosConfigurations = {
        thinkpad = mkNixosConfig {
          hardwareModules = [
            ./modules/hardware/ThinkPad-E595.nix
            # nixos-hardware.nixosModules.lenovo-thinkpad-e595
          ];
          extraModules = [ ./profiles/ThinkPad-E595.nix ];
        };

        probook = mkNixosConfig {
          hardwareModules = [
            ./modules/hardware/ProBook-440G3.nix
            # nixos-hardware.nixosModules.hp-probook-440g3
          ];
          extraModules = [ ./profiles/ProBook-440G3.nix ];
        };
      };

      homeConfigurations = {
        thinkpad = mkHomeConfig {
          username = "sirius";
          extraModules = [ ./profiles/home-manager/ThinkPad-E595.nix ];
        };

        probook = mkHomeConfig {
          username = "orca";
          extraModules = [ ./profiles/home-manager/ProBook-440G3.nix ];
        };

      };
    };

}
