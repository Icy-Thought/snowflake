_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override {
    overrides = final: prev: {
      ewwLib = final.callCabal2nix "ewwLib" ../config/elkowar { };
    };
  };
}
