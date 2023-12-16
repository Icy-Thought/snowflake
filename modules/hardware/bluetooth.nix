{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.hardware.bluetooth = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "bluetooth support";};

  config = mkIf config.modules.hardware.bluetooth.enable {
    user.packages = attrValues {inherit (pkgs) blueman galaxy-buds-client;};

    hardware.bluetooth = {
      enable = true;
      disabledPlugins = ["sap"];
    };

    systemd.user.services.mpris-proxy = {
      description = "mpris-proxy -> bluetooth (media) ctrl";
      after = ["network.target" "sound.target"];
      wantedBy = ["default.target"];
      serviceConfig.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    };
  };
}
