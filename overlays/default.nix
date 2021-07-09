pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      xmonad = let pkg = haskellPackagesNew.callPackage ./xmonad { };
      in pkgs.haskell.lib.dontCheck pkg;
      xmonad-extras = haskellPackagesNew.callPackage ./xmonad-extras { };
      xmonad-contrib =
        let pkg = haskellPackagesNew.callPackage ./xmonad-contrib { };
        in pkgs.haskell.lib.dontCheck pkg;
      xmobar = let pkg = haskellPackagesNew.callPackage ./xmobar { };
      in pkgs.haskell.lib.dontCheck pkg;
    };
  };
}
