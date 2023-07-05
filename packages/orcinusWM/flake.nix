{
  inputs = {
    cargo2nix.url = "github:cargo2nix/cargo2nix/release-0.11.0";
    flake-utils.follows = "cargo2nix/flake-utils";
    nixpkgs.follows = "cargo2nix/nixpkgs";
    rust.url = "github:oxalica/rust-overlay";
  };

  outputs = inputs:
    with inputs;
      flake-utils.lib.eachDefaultSystem (
        system: let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [cargo2nix.overlays.default];
          };

          rustPkgs = pkgs.rustBuilder.makePackageSet {
            rustChannel = "nightly";
            packageFun = import ./default.nix;
            packageOverrides = pkgs:
              pkgs.rustBuilder.overrides.all
              ++ [
                (pkgs.rustBuilder.rustLib.makeOverride {
                  name = "orcinus-wm";
                  overrideAttrs = drv: {
                    propagatedNativeBuildInputs =
                      drv.propagatedNativeBuildInputs
                      or []
                      ++ [
                        pkgs.xorg.libxcb
                        pkgs.xorg.xmodmap
                      ];
                  };
                })
              ];
          };

          workspaceShell = rustPkgs.workspaceShell {
            buildInputs = with pkgs; [
              rust-analyzer
              xorg.libxcb
              xorg.xmodmap
              alejandra
            ];

            nativeBuildInputs = with pkgs; [
              rust-bin.stable.latest.default
            ];
          };
        in rec {
          # Executed by `nix build`
          packages = {
            orcinusWM = (rustPkgs.workspace.orcinus-wm {}).bin;
            default = packages.orcinusWM;
          };

          # Used by `nix develop`
          devShells.default = workspaceShell;

          # Executed by `nix run` :TODO| xephyr script on run
          # apps.default = packages.orcinusWM;
        }
      );
}
