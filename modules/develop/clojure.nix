{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.develop.clojure = { enable = mkBoolOpt false; };

  config = mkMerge [
    (mkIf config.modules.develop.clojure.enable {
      user.packages = attrValues ({ inherit (pkgs) clojure joker leiningen; });
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
