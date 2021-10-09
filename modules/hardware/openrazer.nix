{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.openrazer;
in {
  options.modules.hardware.openrazer = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    hardware.openrazer.enable = true;
    hardware.openrazer.devicesOffOnScreensaver = false;
    hardware.openrazer.syncEffectsEnabled = true;
    hardware.openrazer.mouseBatteryNotifier = true;

    user.extraGroups = [ "plugdev" "openrazer" ];

    environment.systemPackages = with pkgs;
      [
        # TODO: polychromatic
      ];
  };
}
