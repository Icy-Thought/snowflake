{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.X.Y = {
    enable = mkBoolOpt false;
  };

  config = config.modules.X.Y.enable { };
}
