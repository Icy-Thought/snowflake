{
  description = "A structured and configureable NixOS system!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";

    # nixos-hardware.url = "github:NixOS/nixos-hardware";
    # nixos-hardware.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    xmonad.url = "github:xmonad/xmonad";
    xmonad.inputs.nixpkgs.follows = "nixpkgs";

    xmonad-contrib.url = "github:ivanmalison/xmonad-contrib";
    xmonad-contrib.inputs.nixpkgs.follows = "nixpkgs";

    taffybar.url = "github:taffybar/taffybar";
    taffybar.inputs.nixpkgs.follows = "nixpkgs";

    picom-jonaburg.url = "github:jonaburg/picom";
    picom-jonaburg.flake = false;

    rust.url = "github:oxalica/rust-overlay";
    rust.inputs.nixpkgs.follows = "nixpkgs";

    # emacs.url = "github:nix-community/emacs-overlay";
    # emacs.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-mozilla.url = "github:mozilla/nixpkgs-mozilla";
    nixpkgs-mozilla.flake = false;

  };

  outputs = inputs@{ self, nixpkgs, home-manager, flake-utils, agenix, ... }:

    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };

      overlays = [
        inputs.xmonad.overlay
        inputs.xmonad-contrib.overlay
        inputs.agenix.overlay
        inputs.rust.overlay
        # inputs.emacs.overlay

        (import inputs.nixpkgs-mozilla)
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
        , baseModules ? [ ./home/common ], extraModules ? [ ] }:

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
