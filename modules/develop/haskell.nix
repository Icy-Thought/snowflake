{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.develop.haskell = { enable = mkBoolOpt false; };

  config = mkMerge [
    (mkIf config.modules.develop.haskell.enable {
      user.packages = attrValues ({
        inherit (pkgs.haskellPackages)
          cabal-install haskell-language-server hasktags hpack stylish-haskell;
        ghc-with-hoogle = pkgs.haskellPackages.ghcWithHoogle
          (p: with p; [ taffybar xmonad xmonad-contrib ]);
      });

      home.file.ghci-conf = {
        target = ".ghci";
        text = ''
          :set -fobject-code
          :set prompt "\ESC[38;5;3m\STXλ>\ESC[m\STX "
          :set prompt-cont "|> "
          :def hoogle \x -> pure $ ":!hoogle search \"" ++ x ++ "\""
        '';
      };

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
