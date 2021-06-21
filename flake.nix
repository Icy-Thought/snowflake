{
  description = "A structured and configureable NixOS system!";

  inputs = {

    master = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "master";
    };

    unstable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    stable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-21.05";
    };

    home-manager = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
    };

    agenix = {
      type = "github";
      owner = "ryantm";
      repo = "agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly = {
      type = "github";
      owner = "nix-community";
      repo = "neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    naersk = {
      type = "github";
      owner = "nmattia";
      repo = "naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      type = "github";
      owner = "oxalica";
      repo = "rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    # Disabled inputs until further notice:
    #
    # nixos-hardware = {
    #   type = "github";
    #   owner = "NixOS";
    #   repo = "nixos-hardware";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   flake = false;
    # };

    # emacs-overlay = {
    #   type = "github";
    #   owner = "nix-community";
    #   repo = "emacs-overlay";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

  };

  outputs = { self, nixpkgs, home-manager, ... } inputs@: 

  let
    inherit self inputs;

    config = {
        allowUnfree = true;
      };

    overlays = with inputs; [
      (final: _:
        let
          system = final.stdenv.hostPlatform.system;
        in {
          agenix = agenix.defaultPackage.${system};
          neovim-nightly = neovim.packages.${system}.neovim;

          master = import master { inherit config system; };
          unstable = import unstable { inherit config system; };
          stable = import stable { inherit config system; };
      })

    # Imported flakes-overlays
    rust.overlay
  ]

  in
  {
    nixosConfigurations.ThinkPad = import ./hosts/ThinkPad {
      inherit config agenix home inputs nixpkgs overlays;
    };

    ThinkPad = self.nixosConfigurations.superfluous.config.system.build.toplevel;
  };
}
