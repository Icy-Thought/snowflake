{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    ,
    }:
    let
      overlay = import ../../overlays/xmonad.nix;
      overlays = [ overlay xmonad.overlay xmonad-contrib.overlay ];
    in
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
      in
      {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.my-xmonad ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            ghcid
            haskell-language-server
            hlint
            implicit-hie
            stylish-haskell
          ];
        };
        packages.default = pkgs.haskellPackages.my-xmonad;
      }) // { inherit overlay overlays; };
}
