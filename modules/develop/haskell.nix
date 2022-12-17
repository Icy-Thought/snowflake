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
        (ghcWithHoogle (p: with p; [
          taffybar
          xmonad
          xmonad-contrib
        ]))
        cabal-install
        haskell-language-server
        hasktags
        hpack
        stylish-haskell
      ];

      home.file.ghci-conf = {
        target = ".ghci";
        text = ''
          :set -fobject-code
          :set prompt "\ESC[38;5;3m\STXλ>\ESC[m\STX "
          :set prompt-cont "|> "
          :def hoogle \x -> pure $ ":!hoogle search \"" ++ x ++ "\""
        '';
      };
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
