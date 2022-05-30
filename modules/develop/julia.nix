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
  options.modules.develop.julia = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt cfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.julia.enable {
      user.packages = with pkgs; [julia-bin];
      # TODO: automate the installation of: [Plots PyPlot GR UnicodePlots]
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
