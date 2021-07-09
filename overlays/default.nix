pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override {
    overrides = hself: hsuper: rec {
      xmonad = let pkg = hself.callPackage ./xmonad { };
      in pkgs.haskell.lib.dontCheck pkg;
      xmonad-extras = hself.callPackage ./xmonad-extras { };
      xmonad-contrib = let pkg = hself.callPackage ./xmonad-contrib { };
      in pkgs.haskell.lib.dontCheck pkg;
    };
  };

  icy-xmonad = haskellPackages.callPackage ../modules/xmonad/xmonad-config.nix {
    xmonad-extras = haskellPackages.xmonad-extras;
    xmonad-contrib = haskellPackages.xmonad-contrib;
  };
}
