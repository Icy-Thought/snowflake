{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.hardware.powerCtrl = { enable = mkBoolOpt false; };

  config = mkIf config.modules.hardware.powerCtrl.enable {
    services = {
      power-profiles-daemon.enable = false;
      thermald.enable = true;
      tlp = {
        enable = true;
        settings = {
          START_CHARGE_THRESH_BAT0 = 75;
          STOP_CHARGE_THRESH_BAT0 = 80;
          RESTORE_THRESHOLDS_ON_BAT = 1;

          CPU_BOOST_ON_AC = 1;
          CPU_BOOST_ON_BAT = 0;
          CPU_SCALING_GOVERNOR_ON_AC = "performance";
          CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";
        };
      };
    };
  };
}
