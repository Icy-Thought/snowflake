{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  security.rtkit.enable = true;

  service.pipewire.enable = true;
  service.pipewire.alsa = {
    enable = true;
    support32Bit = true;
  };

  service.pipewire.pulse.enable = true;

  # If you want to use JACK applications, uncomment:
  # jack.enable = true;

  # Bluetooth pipewire settings:
  service.pipewire.media-session.config.bluez-monitor.rules = [
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
}
