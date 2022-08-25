{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.clojure;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.clojure = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
        clojure
        joker
        leiningen
      ];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
