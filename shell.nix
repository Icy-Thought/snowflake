{ lib, pkgs ? import <nixpkgs> { }, }:

with lib;
pkgs.mkShell {
  buildInputs = with pkgs; [ git nix-bash-completions ];

  shellHook = let
    nixBin = pkgs.writeShellScriptBin "nix" ''
      ${
        getExe pkgs.nixStable
      } --option experimental-features "nix-command flakes" "$@"
    '';
  in ''
    export FLAKE="$(pwd)"
    export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
  '';
}
