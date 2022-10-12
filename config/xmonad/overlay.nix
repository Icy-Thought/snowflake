_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
        (final: prev: {
          xmonad = final.callCabal2nix "xmonad"
            (pkgs.lib.sourceByRegex ./. [
              "xmonad.hs"
              "xmonad.cabal"
            ])
            { };
        });
  });
}
