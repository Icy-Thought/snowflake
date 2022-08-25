{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.razer;
in {
  options.modules.hardware.razer = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hardware.openrazer = {
      enable = true;
      devicesOffOnScreensaver = false;
      syncEffectsEnabled = true;
      mouseBatteryNotifier = true;
    };

    user.extraGroups = ["plugdev" "openrazer"];

    # GUI for managing your openrazer devices
    user.packages = with pkgs; [polychromatic];
  };
}
