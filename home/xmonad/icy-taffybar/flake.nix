{
  inputs = {
    taffybar.url = "path:./taffybar";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, taffybar, nixpkgs }:

    let
      overlay = import ./overlay.nix;
      overlays = taffybar.overlays ++ [ overlay ];

    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.icy-taffybar p.taffybar ];
        };
        defaultPackage = pkgs.haskellPackages.icy-taffybar;
      }) // {
        inherit overlay overlays;
      };
}
