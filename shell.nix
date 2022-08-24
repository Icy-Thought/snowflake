let
  pkgs = import <nixpkgs> {};
  nixBin = pkgs.writeShellScriptBin "nix" ''
    ${pkgs.nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
  '';
in
  mkShell {
    buildInputs = buitlins.attrValues {
      inherit (pkgs) git nix-bash-completions;
    };

    shellHook = ''
      export FLAKE="$(pwd)"
      export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
    '';
  }
