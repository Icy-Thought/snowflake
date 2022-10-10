_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
        (final: prev: {
          trufflebar =
            final.callCabal2nix "trufflebar"
              (pkgs.lib.sourceByRegex ./. [
                "taffybar.hs"
                "taffybar.css"
                "palette/catppuccin.css"
                "trufflebar.cabal"
              ])
              { };
        });
  });
}
