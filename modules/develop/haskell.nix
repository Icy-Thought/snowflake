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
  codeCfg = config.modules.desktop.editors.vscodium;
in {
  options.modules.develop.haskell = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = [
        pkgs.ghc
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.haskell-language-server
        pkgs.haskellPackages.hasktags
        pkgs.haskellPackages.hoogle
        pkgs.haskellPackages.hpack
        pkgs.haskellPackages.stylish-haskell
      ];

      home.configFile = with config.snowflake; {
        "stylish-haskell/config.yaml" = {
          source = "${configDir}/formatters/stylish-haskell.yaml";
        };
      };
    })

    (mkIf codeCfg.enable {
      hm.programs.vscode.extensions = [
        pkgs.vscode-extensions.haskell.haskell
        pkgs.vscode-extensions.justusadam.language-haskell # syntax-highlighting
      ];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
