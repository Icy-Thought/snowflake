{
  description = "λ well-tailored and configureable NixOS system!";

  nixConfig = {
    substituters = [ "https://cache.nixos.org" ];
    trusted-public-keys =
      [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    # nixos-hardware.url = "github:NixOS/nixos-hardware";
    agenix.url = "github:ryantm/agenix";

    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:icy-thought/xmonad-contrib";
    taffybar.url = "github:taffybar/taffybar";

    picom-jonaburg = {
      url = "github:jonaburg/picom";
      flake = false;
    };

    rust.url = "github:oxalica/rust-overlay";
    # emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, flake-utils, agenix, ... }:

    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };

      overlays = [
        inputs.xmonad.overlay
        inputs.xmonad-contrib.overlay
        inputs.taffybar.overlay

        inputs.agenix.overlay
        inputs.rust.overlay
        # inputs.emacs.overlay

        (final: prev: {
          picom =
            prev.picom.overrideAttrs (_: { src = inputs.picom-jonaburg; });
        })
      ] ++ map (name: import (./overlays + "/${name}"))
        (attrNames (readDir ./overlays));

      lib = nixpkgs.lib.extend (final: prev: home-manager.lib);

      inherit (nixpkgs.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      inherit (builtins) listToAttrs map attrNames readDir;

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
        , baseModules ? [ ./config ], extraModules ? [ ] }:

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
          extraModules = [ ./profiles/home/ThinkPad-E595.nix ];
        };

        probook = mkHomeConfig {
          username = "orca";
          extraModules = [ ./profiles/home/ProBook-440G3.nix ];
        };
      };
    };
}
