{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.envDisplay.lightdm;
in {
  options.modules.envDisplay.lightdm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO: lightdm-webkit2 greeter + theming.
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.greeter.enable = true;
  };
}
