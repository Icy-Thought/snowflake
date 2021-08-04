_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: rec {
        icy-xmonad = self.callCabal2nix "icy-xmonad"
          (pkgs.lib.sourceByRegex ../home/xmonad/icy-xmonad [
            "xmonad.hs"
            "icy-xmonad.cabal"
          ]) { };

        icy-taffybar = self.callCabal2nix "icy-taffybar"
          (pkgs.lib.sourceByRegex ../home/xmonad/icy-taffybar [
            "taffybar.hs"
            "icy-taffybar.cabal"
          ]) { };
      });
  });
}
