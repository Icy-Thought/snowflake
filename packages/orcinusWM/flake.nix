{
  description = "Build a cargo project without extra checks";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    crane,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };

      craneLib = crane.lib.${system};
      orcinusWM = craneLib.buildPackage {
        src = craneLib.cleanCargoSource (craneLib.path ./.);

        buildInputs = with pkgs;
          [binutils]
          ++ lib.optionals pkgs.stdenv.isDarwin [libiconv];
        # Additional environment variables can be set directly
        # MY_CUSTOM_VAR = "some value";
      };
    in {
      checks = {
        inherit orcinusWM;
      };

      packages.default = orcinusWM;

      apps.default = flake-utils.lib.mkApp {
        drv = orcinusWM;
      };

      devShells.default = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.checks.${system};

        # Additional dev-shell environment variables can be set directly
        # MY_CUSTOM_DEVELOPMENT_VAR = "something else";

        # Extra inputs can be added here
        nativeBuildInputs = with pkgs; [
          cargo
          rustc
        ];
      };
    });
}
