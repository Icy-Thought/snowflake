{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.hardware.razer = { enable = mkBoolOpt false; };

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
