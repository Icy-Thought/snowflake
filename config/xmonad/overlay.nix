_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (final: prev: {
        birostrisWM = final.callCabal2nix "birostrisWM"
          (pkgs.lib.sourceByRegex ./. [ "xmonad.hs" "birostrisWM.cabal" ]) { };
      });
  });
}
