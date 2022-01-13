{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop.haskell;
in {
  options.modules.develop.haskell = {
    enable = mkBoolOpt false;
    dhall.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = with pkgs;
        [
          (ghc.withHoogle (self:
            with self; [
              cabal2nix
              cabal-install
              hie-bios
              hlint
              implicit-hie
              # stack
            ]))
        ];
    }

    (mkIf cfg.dhall.enable {
      user.packages = with pkgs; [
        haskellPackages.dhall
        haskellPackages.dhall-json
      ];
    })
  ]);
}
