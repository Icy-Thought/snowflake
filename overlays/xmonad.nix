_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
      (self: super: rec {
        my-xmonad =
          self.callCabal2nix "my-xmonad"
          (pkgs.lib.sourceByRegex ../config/my-xmonad [
            "xmonad.hs"
            "my-xmonad.cabal"
          ]) {};
      });
  });
}
