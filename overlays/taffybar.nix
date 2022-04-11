_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: rec {
        my-taffybar = self.callCabal2nix "my-taffybar"
          (pkgs.lib.sourceByRegex ../config/my-taffybar [
            "taffybar.hs"
            "taffybar.css"
            "my-palette.css"
            "my-taffybar.cabal"
          ]) { };
      });
  });
}
