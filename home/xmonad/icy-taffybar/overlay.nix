_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: rec {
        icy-taffybar = self.callCabal2nix "icy-taffybar"
        (pkgs.lib.sourceByRegex ./. [
            "taffybar.css"
            "taffybar.hs"
            "icy-taffybar.cabal"
          ]) { };
      });
  });
}
