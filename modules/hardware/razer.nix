{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.hardware.razer = {
    enable = mkEnableOption "razer-device manager";
  };

  config = mkIf config.modules.hardware.razer.enable {
    hardware.openrazer = {
      enable = true;
      devicesOffOnScreensaver = false;
      syncEffectsEnabled = true;
      batteryNotifier = {
        enable = true;
        percentage = 30;
        frequency = 600;
      };
    };

    user.extraGroups = [ "openrazer" ];

    # GUI for managing your openrazer devices
    user.packages = [ pkgs.polychromatic ];
  };
}
