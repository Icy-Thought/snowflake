_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: rec {
        icy-xmonad = self.callCabal2nix "icy-xmonad"
          (pkgs.lib.sourceByRegex ../config/icy-xmonad [
            "xmonad.hs"
            "icy-xmonad.cabal"
          ]) { };
      });
  });
}
