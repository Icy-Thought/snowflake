{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.hardware.bluetooth = { enable = mkBoolOpt false; };

  config = mkIf config.modules.hardware.bluetooth.enable {
    user.packages = attrValues ({ inherit (pkgs) blueman galaxy-buds-client; });

    hardware.bluetooth = {
      enable = true;
      disabledPlugins = [ "sap" ];
    };
  };
}
