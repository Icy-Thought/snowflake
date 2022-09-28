{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [ git nix-bash-completions ];

  shellHook =
    let
      nixBin = pkgs.writeShellScriptBin "nix" ''
        ${pkgs.nixVersions.stable}/bin/nix --option experimental-features "nix-command flakes" "$@"
      '';
    in
    ''
      export FLAKE="$(pwd)"
      export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
    '';
}
