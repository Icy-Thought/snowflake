_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (final: prev: rec {
        icy-xmonad = final.callCabal2nix "icy-xmonad"
          (../modules/window-managers/xmonad/config/xmonad) { };
        xmonad = final.callCabal2nix "xmonad" (pkgs.fetchFromGitHub {
          owner = "xmonad";
          repo = "xmonad";
          rev = "05aeef0dc2ef84d92f2d3dec6cd3acdecb4c9851";
          sha256 = "vRxjhRJqu5N4/+Xd4prxssjC4bYvLmpJpSbtUYCCYBo=";
        }) { };
        xmonad-contrib = final.callCabal2nix "xmonad-contrib"
          (pkgs.fetchFromGitHub {
            owner = "xmonad";
            repo = "xmonad-contrib";
            rev = "4c759ff70cd2105cfe39bd682056f4890001a844";
            sha256 = "SwLgUOEa5TPaS5vYx2CAwC15yNlziMYAkNfSUNOpAjg=";
          }) { };
      });
  });
}
