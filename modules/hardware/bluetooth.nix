{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.hardware.bluetooth = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "bluetooth support"; };

  config = mkIf config.modules.hardware.bluetooth.enable {
    user.packages = attrValues { inherit (pkgs) galaxy-buds-client; };

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
