_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
      (final: prev: rec {
        xmonad =
          final.callCabal2nix "xmonad"
          (pkgs.lib.sourceByRegex ./. [
            "xmonad.hs"
            "xmonad.cabal"
          ]) {};
      });
  });
}
