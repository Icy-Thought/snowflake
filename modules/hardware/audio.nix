{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = {
    enable = mkBoolOpt false;
    pulse.enable = mkBoolOpt true;
    alsa.enable = mkBoolOpt true;
    jack.enable = mkBoolOpt false;
    bluetooth.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      security.rtkit.enable = true;
      services.pipewire.enable = true;
    }

    (mkIf cfg.pulse.enable { services.pipewire.pulse.enable = true; })

    (mkIf cfg.alsa.enable {
      services = {
        pipewire.alsa.enable = true;
        pipewire.alsa.support32Bit = true;
      };
    })

    (mkIf cfg.jack.enable { services.pipewire.jack.enable = true; })

    (mkIf cfg.bluetooth.enable {
      hardware.bluetooth.enable = true;

      services.pipewire.media-session.config.bluez-monitor.rules = [
        {
          # Matches all cards
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
            # Matches all sources
            {
              "node.name" = "~bluez_input.*";
            }
            # Matches all outputs
            { "node.name" = "~bluez_output.*"; }
          ];
          actions = { "node.pause-on-idle" = false; };
        }
      ];
    })
  ]);
}
