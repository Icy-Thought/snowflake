{ config
, options
, lib
, pkgs
, ...
}:

let
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in
{
  options.modules.develop.clojure = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf config.modules.develop.clojure.enable {
      user.packages = with pkgs; [ clojure joker leiningen ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
