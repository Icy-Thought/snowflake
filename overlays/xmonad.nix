self: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = with self.haskell.lib;
      self.lib.composeExtensions (old.overrides or (_: _: { }))
      (hself: hsuper: {
        xmonad = dontCheck (overrideSrc hsuper.xmonad {
          version = "0.16.9999";
          src = self.fetchFromGitHub {
            owner = "xmonad";
            repo = "xmonad";
            rev = "af354f7528ada1de451365a0f5138ef10a318360";
            sha256 = "sq+hDiMfzfYRvzYucpmNt4u60QT9HXH+rJ89jptyMSI=";
            fetchSubmodules = true;
          };
        });

        xmonad-extras = dontCheck (overrideSrc hsuper.xmonad-extras {
          version = "0.15.3";
          src = self.fetchFromGitHub {
            owner = "xmonad";
            repo = "xmonad-extras";
            rev = "6df82de88474754bc90724251d5fcbeccccbd7e7";
            sha256 = "1kj8xzp7d8y0w63r46zvgav6a3320c6blsilaldaylgqb10h6aga";
            fetchSubmodules = false;
          };
        });

        xmonad-contrib = dontCheck (overrideSrc hsuper.xmonad-contrib {
          version = "0.17";
          src = self.fetchFromGitHub {
            owner = "xmonad";
            repo = "xmonad-contrib";
            rev = "1351f9a931f53e9f1e16c566c70cb8fa98f97785";
            sha256 = "ZX7YU/mp/ORufbL4whnD1vBXVcMqOv8aN+x+lQ7HdOo=";
            fetchSubmodules = true;
          };
        });
      });
  });
}
