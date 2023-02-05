_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (final: prev: {
        birostrisWM = final.callCabal2nix "birostrisWM"
          (pkgs.lib.sourceByRegex ../config/xmonad [
            "xmonad.hs"
            "birostrisWM.cabal"
          ]) { };
      });
  });
}
