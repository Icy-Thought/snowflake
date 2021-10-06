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
    # emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, flake-utils, agenix, ... }:

    let
      inherit (nixpkgs.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      inherit (builtins) listToAttrs map attrNames readDir;

      lib = nixpkgs.lib.extend (final: prev: home-manager.lib);

      pkgs = import nixpkgs { system = "x86_64-linux"; };

      overlays = [
        inputs.xmonad.overlay
        inputs.xmonad-contrib.overlay
        inputs.taffybar.overlay

        inputs.agenix.overlay
        inputs.rust.overlay
        # inputs.emacs.overlay

        (final: prev: {
          master = import inputs.nixpkgs-master {
            system = prev.system;
            config.allowUnfree = true;
          };

          picom =
            prev.picom.overrideAttrs (_: { src = inputs.picom-jonaburg; });
        })
      ] ++ map (name: import (./overlays + "/${name}"))
        (attrNames (readDir ./overlays));

      # Generate default NixOS config
      mkNixosConfig = { system ? "x86_64-linux", hardwareModules
        , nixpkgs ? inputs.nixpkgs, master ? inputs.nixpkgs-master
        , baseModules ? [
          agenix.nixosModules.age
          home-manager.nixosModules.home-manager
          ./modules/nixos
        ], extraModules ? [ ] }:

        nixosSystem {
          inherit system;
          modules = baseModules ++ hardwareModules ++ extraModules
            ++ [{ nixpkgs.overlays = overlays; }];
          specialArgs = { inherit inputs lib nixpkgs master; };
        };

      # Generate default Home-Manager conf
      mkHomeConfig = { username, system ? "x86_64-linux"
        , nixpkgs ? inputs.nixpkgs, master ? inputs.nixpkgs-master
        , baseModules ? [ ./config ], extraModules ? [ ] }:

        homeManagerConfiguration rec {
          inherit system username;
          homeDirectory = "/home/${username}";
          extraSpecialArgs = { inherit inputs lib nixpkgs master; };
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
