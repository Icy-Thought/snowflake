{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.develop.haskell = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf config.modules.develop.haskell.enable {
      user.packages = with pkgs.haskellPackages; [
        ghc
        brittany
        cabal-install
        haskell-language-server
        hasktags
        hoogle
        hpack
      ];
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions = with pkgs.vscode-extensions; [
        haskell.haskell
        justusadam.language-haskell # syntax-highlighting
      ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
