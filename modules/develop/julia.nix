{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.julia;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.julia = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [julia-bin];
      # TODO: automate the installation of: [Plots PyPlot GR UnicodePlots]
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
