{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.laptop;
in {
  options.modules.services.laptop = { basics = mkBoolOpt false; };

  config = mkIf cfg.basics {
    services.upower.enable = true;
    services.printing.enable = true;
  };
}
