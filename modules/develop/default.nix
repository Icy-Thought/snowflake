{ config, options, lib, pkgs, ... }:

with lib; {
  options.modules.develop = {
    xdg.enable = mkEnableOption "XDG-related conf" // { default = true; };
  };

  config = mkIf config.modules.develop.xdg.enable {
    # TODO:
  };
}
