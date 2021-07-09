self: super:

let
  xmonadGH = self.fetchFromGitHub {
    owner = "xmonad";
    repo = "xmonad";
    rev = "af354f7528ada1de451365a0f5138ef10a318360";
    sha256 = "sq+hDiMfzfYRvzYucpmNt4u60QT9HXH+rJ89jptyMSI=";
    fetchSubmodules = true;
  };

  xmonad-extrasGH = self.fetchFromGitHub {
    owner = "xmonad";
    repo = "xmonad-extras";
    rev = "6df82de88474754bc90724251d5fcbeccccbd7e7";
    sha256 = "1kj8xzp7d8y0w63r46zvgav6a3320c6blsilaldaylgqb10h6aga";
    fetchSubmodules = false;
  };

  xmonad-contribGH = self.fetchFromGitHub {
    owner = "xmonad";
    repo = "xmonad-contrib";
    rev = "1351f9a931f53e9f1e16c566c70cb8fa98f97785";
    sha256 = "ZX7YU/mp/ORufbL4whnD1vBXVcMqOv8aN+x+lQ7HdOo=";
    fetchSubmodules = true;
  };

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: { }))
      (hself: hsuper: rec {
        xmonad = hself.callCabal2nix "xmonad" xmonadGH { };
        xmonad-extras = hself.callCabal2nix "xmonad-extras" xmonad-extrasGH { };
        xmonad-contrib =
          hself.callCabal2nix "xmonad-contrib" xmonad-contribGH { };
        xmonad-config =
          hself.callPackage ../modules/xmonad/xmonad-config.nix { };
      });
  });
}
