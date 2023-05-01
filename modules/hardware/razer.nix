{ config, options, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.hardware.razer = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "razer-device manager"; };

  config = mkIf config.modules.hardware.razer.enable {
    hardware.openrazer = {
      enable = true;
      devicesOffOnScreensaver = false;
      syncEffectsEnabled = true;
      mouseBatteryNotifier = true;
    };

    user.extraGroups = [ "plugdev" "openrazer" ];

    # GUI for managing your openrazer devices
    user.packages = [ pkgs.polychromatic ];
  };
}
