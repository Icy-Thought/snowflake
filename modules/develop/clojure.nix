{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop;
in {
  options.modules.develop.clojure = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt cfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.clojure.enable {
      user.packages = with pkgs; [clojure joker leiningen];
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
