{ inputs, config, lib, pkgs, ... }:
{
  imports = [ 
      ./primary.nix
      ./nixpkgs.nix
      ./wireguard.nix
    ];

  etc = {
    home-manager.source = "${inputs.home-manager}";
    nixpkgs.source = "${inputs.nixpkgs}";
  };

  user = {
    description = "icy-thought";
    home = "/home/${config.user.name}";
    shell = pkgs.fish;
  };

  users = { # Don't forget to set password through `passwd`!
    defaultUserShell = pkgs.fish;
    mutableUsers = false;
    
    users = {
      "${config.user.name}" = {
        isNormalUser = true;
        createHome = true;
        useDefaultShell = true;
        extraGroups = [ "wheel" "network" "plugdev" "adbusers" ];
        hashedPassword = "$6$DMQjZ0Nn8JAb$2MBYjRZvhACwUJrDXI6GciNglr.KM3Yaza4CMUaG8HCxOJ2EtRqZZKvTBzRhIPQWjKiYeU3cCpntQNkToiUeu0";
      };
    };
  };

  home-manager = {
    extraSpecialArgs = { inherit inputs lib; };
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  systemd.services = {
    systemd-resolved.enable = true;
    systemd-machined.enable = false;
    upower.enable = true;
  };

  services = {
    printing = {
      enable = true;
    };

    xserver = {
      enable = true;
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
    };

    flatpak = {
      enable = true;
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
