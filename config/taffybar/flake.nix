{
  inputs = {
    taffybar.url = "github:taffybar/taffybar";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs, taffybar }:
    let
      overlay = import ../../overlays/taffybar.nix;
      overlays = taffybar.overlays ++ [ overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
      in {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.raybar ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            ghcid
            haskell-language-server
            hlint
            implicit-hie
            stylish-haskell
          ];

        };
        packages.default = pkgs.haskellPackages.raybar;
      }) // {
        inherit overlay overlays;
      };
}
