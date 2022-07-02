_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
      (final: prev: rec {
        my-taffybar =
          final.callCabal2nix "my-taffybar"
          (pkgs.lib.sourceByRegex ../config/my-taffybar [
            "taffybar.hs"
            "taffybar.css"
            "catppuccin.css"
            "my-taffybar.cabal"
          ]) {};
      });
  });
}
