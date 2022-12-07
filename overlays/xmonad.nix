_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
        (final: prev: {
          birostris-WM =
            final.callCabal2nix "birostris"
              (pkgs.lib.sourceByRegex ../config/xmonad [ "xmonad.hs" "birostris.cabal" ])
              { };
        });
  });
}
