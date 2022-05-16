{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop;
in {
  options.modules.develop.haskell = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt cfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.haskell.enable {
      user.packages = with pkgs;
        [ghc]
        ++ (with haskellPackages; [
          cabal-install
          haskell-language-server
          hasktags
          hoogle
          hpack
          stylish-haskell
        ]);
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
