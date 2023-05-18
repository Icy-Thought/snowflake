{
  inputs = {
    taffybar.url = "github:taffybar/taffybar";
    flake-utils.url = "github:numtide/flake-utils";
    xmonad.url = "github:xmonad/xmonad/master";
  };
  outputs = { self, flake-utils, taffybar, nixpkgs, xmonad }:
    let
      hoverlay = final: prev: hself: hsuper: {
        raybar = prev.haskell.lib.addPkgconfigDepends
          (hself.callCabal2nix "raybar" ./. { }) [
            final.util-linux.dev
            final.pcre2
            final.pcre
            final.libselinux.dev
            final.libsepol.dev
            final.fribidi.out
            final.fribidi.dev
            final.libthai.dev
            final.libdatrie.dev
            final.xorg.libXdmcp.dev
            final.libxkbcommon.dev
            final.libepoxy.dev
            final.xorg.libXtst.out
          ];
      };
      defComp = { compiler = "ghc94"; };
      overlay = xmonad.lib.fromHOL hoverlay defComp;
      overlays = [ taffybar.overlay overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
        hpkgs =
          pkgs.lib.attrsets.getAttrFromPath (xmonad.lib.hpath defComp) pkgs;
      in {
        devShell = hpkgs.shellFor {
          packages = p: [ p.raybar p.taffybar ];
          nativeBuildInputs = with hpkgs;
            [
              cabal-install
              # ghcid ormolu implicit-hie haskell-language-server hlint
            ];
        };
        defaultPackage = hpkgs.raybar;
      }) // {
        inherit overlay overlays;
      };
}
