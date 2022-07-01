_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
      (final: prev: rec {
        my-xmonad =
          final.callCabal2nix "my-xmonad"
          (pkgs.lib.sourceByRegex ../config/my-xmonad [
            "xmonad.hs"
            "my-xmonad.cabal"
          ]) {};
      });
  });
}
