{
  lib,
  pkgs ? import <nixpkgs> {},
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.meta) getExe;
in
  pkgs.mkShell {
    buildInputs = attrValues {inherit (pkgs) git nix-bash-completions;};

    shellHook = let
      inherit (pkgs) nixStable writeShellScriptBin;
      nixBin = writeShellScriptBin "nix" ''
        ${
          getExe nixStable
        } --option experimental-features "nix-command flakes" "$@"
      '';
    in ''
      export FLAKE="$(pwd)"
      export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
    '';
  }
