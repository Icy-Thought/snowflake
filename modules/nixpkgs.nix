{ inputs, config, lib, pkgs, ... }:
{
  nixpkgs = {
    config = import ./config.nix;
    overlays = [ ];
  };

  nix = {
    package = pkgs.nixFlakes;

    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      ${lib.optionalString (config.nix.package == pkgs.nixFlakes)
      "experimental-features = nix-command flakes"}
    '';

    trustedUsers = [ "${config.user.name}" "root" "@admin" "@wheel" ];

    gc = {
      automatic = true;
      options = "--delete-older-than 5d";
    };

    buildCores = 8;
    maxJobs = 8;
    readOnlyStore = true;

    nixPath = [
      "nixpkgs=/etc/${config.environment.etc.nixpkgs.target}"
      "home-manager=/etc/${config.environment.etc.home-manager.target}"
    ];

    binaryCaches = [
      "https://nix-community.cachix.org/"
    ];

    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    registry = {
      master = {
        from = {
          id = "master";
          type = "indirect";
        };
        flake = inputs.master;
      };

      nixpkgs = {
        from = {
          id = "nixpkgs";
          type = "indirect";
        };
        flake = inputs.nixpkgs;
      };
    };
  };

}
