{
  description = "A structured and configureable NixOS system!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
    # nixos-hardware.url = "github:NixOS/nixos-hardware";
    agenix.url = "github:ryantm/agenix";

    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:ivanmalison/xmonad-contrib";

    picom-jonaburg = {
      url = "github:jonaburg/picom";
      flake = false;
    };

    rust.url = "github:oxalica/rust-overlay";
    # emacs.url = "github:nix-community/emacs-overlay";

    nixpkgs-mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };

  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, home-manager, flake-utils
    , agenix, ... }:

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

      inherit self inputs;
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
