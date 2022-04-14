{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop;
in {
  options.modules.develop.shell = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt cfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.shell.enable { user.packages = with pkgs; [ shellcheck ]; })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
