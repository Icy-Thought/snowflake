{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  hardware.openrazer.enable = true;
  hardware.openrazer.devicesOffOnScreensaver = false;
  hardware.openrazer.syncEffectsEnabled = true;
  hardware.openrazer.mouseBatteryNotifier = true;

  user.extraGroups = [ "plugdev" "openrazer" ];

  environment.systemPackages = with pkgs;
    [
      # TODO: polychromatic
    ];
}
