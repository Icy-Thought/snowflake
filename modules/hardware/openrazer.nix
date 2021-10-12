{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.openrazer;
in {
  options.modules.hardware.openrazer = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    hardware.openrazer = {
      enable = true;
      devicesOffOnScreensaver = false;
      syncEffectsEnabled = true;
      mouseBatteryNotifier = true;
    };

    user.extraGroups = [ "plugdev" "openrazer" ];

    environment.systemPackages = with pkgs;
      [
        # TODO: polychromatic
      ];
  };
}
