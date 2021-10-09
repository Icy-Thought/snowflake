{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.envDisplay.gdm;
in {
  options.modules.envDisplay.gdm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.displayManager.gdm.wayland = true;
  };
}
