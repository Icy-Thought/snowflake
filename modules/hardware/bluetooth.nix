{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.hardware.bluetooth = {
    enable = mkEnableOption "bluetooth support";
  };

  config = mkIf config.modules.hardware.bluetooth.enable {
    user.packages = [ pkgs.galaxy-buds-client ];

    services.blueman.enable = true;

    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings.General = {
        ControllerMode = "bredr";
        Experimental = true;
      };
    };

    systemd.user.services.mpris-proxy = {
      description = "mpris-proxy -> bluetooth (media) ctrl";
      after = [ "network.target" "sound.target" ];
      wantedBy = [ "default.target" ];
      serviceConfig.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    };
  };
}
