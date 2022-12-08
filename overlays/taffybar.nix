_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
        (final: prev: {
          raybar = final.callCabal2nix "raybar"
            (pkgs.lib.sourceByRegex ../config/taffybar [
              "taffybar.hs"
              "taffybar.css"
              "palette/tokyonight.css"
              "raybar.cabal"
            ])
            { };
        });
  });
}
