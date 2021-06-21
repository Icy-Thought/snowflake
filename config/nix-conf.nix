{ inputs, system, nixpkgs }:

rec {
  binaryCaches = [
    "https://cache.nixos.org?priority=10"
    "https://cache.ngi0.nixos.org/"
    "https://nix-community.cachix.org"
  ];

  binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

  daemonNiceLevel = 1;
  daemonIONiceLevel = 1;

  extraOptions = ''
    experimental-features = ca-references ca-derivations nix-command flakes
    keep-outputs = true
    keep-derivations = true
  '';

  gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  maxJobs = 4;

  nixPath =
    let path = toString ../.;
    in
    [
      "repl=${path}/repl.nix"
      "nixpkgs=${nixpkgs}"
      "home-manager=${inputs.home}"
    ];

  optimise = {
    automatic = true;
    dates = [ "03:00" ];
  };

  package = nixpkgs.legacyPackages."${system}".nixFlakes;

  registry = {
    system.flake = inputs.self;
    default.flake = nixpkgs;
    home-manager.flake = inputs.home;
  };

  trustedBinaryCaches = binaryCaches;
  trustedUsers = [ "root" "sirius" ];
}
