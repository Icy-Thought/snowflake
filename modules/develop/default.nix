{ config
, options
, lib
, pkgs
, ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in
{
  options.modules.develop = {
    xdg.enable = mkBoolOpt true;
  };

  config = mkIf config.modules.develop.xdg.enable {
    # TODO:
  };
}
