{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.laptop;
in {
  options.modules.services.laptop = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services = {
      upower.enable = true;
      printing.enable = true;
    };
  };
}
