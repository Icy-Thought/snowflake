{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop;
in {
  options.modules.develop = { xdg.enable = mkBoolOpt true; };

  config = mkIf cfg.xdg.enable {
    # TODO
  };
}
