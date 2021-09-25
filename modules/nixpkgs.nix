{ inputs, config, lib, pkgs, ... }: {

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = false;
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
      options = "--delete-older-than 3d";
    };

    autoOptimiseStore = true;

    buildCores = 8;
    maxJobs = 8;
    readOnlyStore = true;

    nixPath = [
      "nixpkgs=/etc/${config.environment.etc.nixpkgs.target}"
      "home-manager=/etc/${config.environment.etc.home-manager.target}"
    ];

    binaryCaches = [ "https://cache.nixos.org/" ];
    binaryCachePublicKeys =
      [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];

    registry = { nixpkgs.flake = inputs.nixpkgs; };

  };

}
