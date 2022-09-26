{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
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
