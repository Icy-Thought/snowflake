{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.envDisplay.lightdm;
in {
  options.modules.desktop.envDisplay.lightdm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO: lightdm-webkit2 greeter + theming.
    services.xserver = {
      enable = true;

      displayManager.lightdm = {
        enable = true;
        greeter.enable = true;
      };
    };
  };
}
