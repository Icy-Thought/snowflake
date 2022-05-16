{
  inputs = {
    taffybar.url = "github:icy-thought/taffybar";
    # taffybar.url = "github:taffybar/taffybar";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    flake-utils,
    taffybar,
    nixpkgs,
  }: let
    overlay = import ../../overlays/taffybar.nix;
    overlays = taffybar.overlays ++ [overlay];
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system overlays;
        config.allowBroken = true;
      };
    in rec {
      devShell =
        pkgs.haskellPackages.shellFor {packages = p: [p.my-taffybar];};
      defaultPackage = pkgs.haskellPackages.my-taffybar;
    })
    // {
      inherit overlay overlays;
    };
}
