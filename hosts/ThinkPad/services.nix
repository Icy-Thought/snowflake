{ config, pkgs, ... }:
{
  systemd.services = {
    systemd-resolved.enable = true;
    systemd-machined.enable = false;
    upower.enable = true;
  };

  services = {
    avahi.enable = false;

    gnome = {
      gnome-keyring.enable = true;
    };

    printing = {
      enable = true;
    };

    xserver = {
      enable = true;
      videoDrivers = [ "amdgpu"];

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
    
      displayManager.gdm = {
        enable  = true;
        wayland = true;
      };

      desktopManager.gnome = {
        enable = true;
      };
   };

    flatpak = {
      enable = true;
    };

    dbus = {
      enable = true;
      packages = with pkgs; [ 
        gnome.dconf 
      ];
    };

    udev = {
      extraRules = ''
        ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
        ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
        ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
      '';

      packages = with pkgs; [
        gnome.gnome-settings-daemon 
      ];

    };

    gnome = {
      chrome-gnome-shell.enable = true;
    };

    gvfs = {
      enable = true;
    };

    mpd = {
      enable = false;
      extraConfig = builtins.readFile ../../nixpkgs/config/mpd.conf;
    };

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
  };

}
