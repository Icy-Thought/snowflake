{ config, pkgs, ... }:

{
  services = {
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };

      pulse = {
        enable = true;
      };

      # If you want to use JACK applications, uncomment:
      # #jack.enable = true;

      # Bluetooth pipewire settings:
      media-session.config.bluez-monitor.rules = [
        {
          # Matches all cards
          matches = [ { "device.name" = "~bluez_card.*"; } ];
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
            { "node.name" = "~bluez_input.*"; }
            # Matches all outputs
            { "node.name" = "~bluez_output.*"; }
          ];
          actions = {
            "node.pause-on-idle" = false;
          };
        }
      ];
    };

    printing = {
      enable = true;
    };

    xserver = {
      enable = true;
      videoDrivers = [ "modesetting" ];
      useGlamor = true;

      layout = "us";
      xkbOptions = "eurosign:e";

      libinput = {
        enable = true;
        touchpad = {
          naturalScrolling = true;
          tapping = true;
          disableWhileTyping = true;
        };
      };

      displayManager.sddm = {
        enable = true;
      };

      desktopManager.plasma5 = {
        enable = true;
      };
    };

    gvfs = {
      enable = true;
    };

    mpd = {
      enable = false;
      extraConfig = builtins.readFile ../../nixpkgs/config/mpd.conf;
    };

  };

}
