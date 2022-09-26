{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.develop = {
    xdg.enable = mkBoolOpt true;
  };

  config = mkIf config.modules.develop.xdg.enable {
    # TODO:
  };
}
