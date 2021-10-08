{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop.haskell;
in {
  options.modules.develop.haskell = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ghc
      cabal-install
      haskellPackages.hoogle
      cabal2nix
    ];
  };
}
