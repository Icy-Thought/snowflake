{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.haskell;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.haskell = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
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

      home.programs.vscode.extensions = with pkgs.vscode-extensions; [
        haskell.haskell
        justusadam.language-haskell # syntax-highlighting
      ];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
