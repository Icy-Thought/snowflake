_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (final: prev: {
        raybar = final.callCabal2nix "raybar"
          (pkgs.lib.sourceByRegex ./. [ "taffybar.hs" "raybar.cabal" ]) { };
      });
  });
}
