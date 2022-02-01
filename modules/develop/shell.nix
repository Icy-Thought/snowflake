{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let
  devCfg = config.modules.develop;
  cfg = devCfg.shell;
in {
  options.modules.develop.shell = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt devCfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.enable { user.packages = with pkgs; [ shellcheck ]; })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
