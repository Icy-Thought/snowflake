{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.envDisplay.gdm;
in {
  options.modules.desktop.envDisplay.gdm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.xserver.displayManager = {
      gdm.enable = true;
      gdm.wayland = true;
    };
  };
}
