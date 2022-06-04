{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.xdg;
in {
  options.modules.develop = {
    xdg.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    # TODO:
  };
}
