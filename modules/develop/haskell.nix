{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.develop.haskell = {
    enable = mkEnableOption "Haskell development";
  };

  config = mkIf config.modules.develop.haskell.enable (mkMerge [
    {
      user.packages = with pkgs.haskellPackages; [
        cabal-install
        fourmolu
        haskell-language-server
        hasktags
        hpack
        (ghcWithHoogle (p: with p; [ taffybar xmonad xmonad-contrib ]))
      ];
    }

    (mkIf config.modules.develop.xdg.enable {
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
  ]);
}
