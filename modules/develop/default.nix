{ config, options, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.develop = let inherit (lib.options) mkEnableOption;
  in { xdg.enable = mkEnableOption true; };

  config = mkIf config.modules.develop.xdg.enable {
    # TODO:
  };
}
