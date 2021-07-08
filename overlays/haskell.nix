self: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = with self.haskell.lib;
      self.lib.composeExtensions (old.overrides or (_: _: { }))
      (hself: hsuper: {

        xmonad = overrideSrc hsuper.xmonad {
          src = self.fetchFromGitHub {
            owner = "xmonad";
            repo = "xmonad";
            rev = "af354f7528ada1de451365a0f5138ef10a318360";
            sha256 = "sq+hDiMfzfYRvzYucpmNt4u60QT9HXH+rJ89jptyMSI=";
          };
        };
        xmonad-contrib = overrideSrc hsuper.xmonad-contrib {
          src = self.fetchFromGitHub {
            owner = "xmonad";
            repo = "xmonad-contrib";
            rev = "1351f9a931f53e9f1e16c566c70cb8fa98f97785";
            sha256 = "ZX7YU/mp/ORufbL4whnD1vBXVcMqOv8aN+x+lQ7HdOo=";
          };
        };
      });
  });
}
