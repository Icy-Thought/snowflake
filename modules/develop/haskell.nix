{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.haskell = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Haskell development"; };

  config = mkIf config.modules.develop.haskell.enable (mkMerge [
    {
      user.packages = attrValues {
        inherit (pkgs.haskellPackages)
          cabal-install fourmolu haskell-language-server hasktags hpack;
        ghc-with-hoogle = pkgs.haskellPackages.ghcWithHoogle
          (p: with p; [ taffybar xmonad xmonad-contrib ]);
      };
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
