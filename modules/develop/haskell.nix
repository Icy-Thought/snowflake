{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  devCfg = config.modules.develop;
  cfg = devCfg.haskell;
in {
  options.modules.develop.haskell = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt devCfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.enable {
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
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
