{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.r-language;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.r-language = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs;
        [R]
        ++ (with rPackages; [
          languageserver
          tidyverse
        ]);
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
