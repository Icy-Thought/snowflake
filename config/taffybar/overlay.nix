_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
      (final: prev: rec {
        taffybar =
          final.callCabal2nix "taffybar"
          (pkgs.lib.sourceByRegex ./. [
            "taffybar.hs"
            "taffybar.css"
            "catppuccin.css"
            "taffybar.cabal"
          ]) {};
      });
  });
}
