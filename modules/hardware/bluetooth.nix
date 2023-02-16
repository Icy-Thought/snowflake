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
      package = pkgs.bluez;
      powerOnBoot = true;
      hsphfpd.enable = false;
      disabledPlugins = [ "sap" ];
      settings = {
        # Xbox X Controller Settings:
        General = {
          Class = "0x000100";
          FastConnectable = true;
          JustWorksRepairing = "always";
          Privacy = "device";
        };
      };
    };

    services.pipewire.media-session.config.bluez-monitor.rules = [
      {
        matches = [{ "device.name" = "~bluez_card.*"; }];
        actions = {
          "update-props" = {
            "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
            # mSBC is not expected to work on all headset + adapter combinations.
            "bluez5.msbc-support" = true;
          };
        };
      }
      {
        matches = [
          { "node.name" = "~bluez_input.*"; }
          { "node.name" = "~bluez_output.*"; }
        ];
        actions = { "node.pause-on-idle" = false; };
      }
    ];
  };
}
