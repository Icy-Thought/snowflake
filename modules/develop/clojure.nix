{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.clojure = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "Clojure development";};

  config = mkMerge [
    (mkIf config.modules.develop.clojure.enable {
      user.packages = attrValues {inherit (pkgs) clojure joker leiningen;};
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
