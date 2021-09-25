{ inputs, config, lib, pkgs, ... }: {

  # Build NixOS from latest stable release.
  system.stateVersion = "21.05"; # Did you read the comment?

  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    kernelParams = [ "pcie_aspm.policy=performance" ];

    # Set GRUB2 to default boot.
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };

      grub = {
        enable = true;
        version = 2;
        efiSupport = true;
        device = "nodev";
        useOSProber = true;
      };
    };

    cleanTmpDir = true;
  };

  networking = {
    # Global useDHCP => deprecated.
    useDHCP = false;

    # Per-interface useDHCP is mandatory. (Not Required by NetworkManager)
    # interfaces = {
    #   enp1s0.useDHCP = true;
    #   wlan0.useDHCP = true;
    # };

    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };

    nat = {
      enable = true;
      externalInterface = "eth0";
      internalInterfaces = [ "wg0" ];
    };

    iproute2.enable = true;

    firewall = {
      # 443 = openssl, 51820 = Wireguard, 1714 - 1764 = KDE-Connect
      allowedTCPPorts = [ 53 443 ];
      allowedUDPPorts = [ 53 51820 ];
      allowedTCPPortRanges = [{
        from = 1714;
        to = 1764;
      }];

      allowedUDPPortRanges = [{
        from = 1714;
        to = 1764;
      }];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  time.timeZone = "Europe/Berlin";

  users = { # Don't forget to set password through `passwd`!
    defaultUserShell = pkgs.fish;
    mutableUsers = false;

    users = {
      "${config.user.name}" = {
        isNormalUser = true;
        createHome = true;
        useDefaultShell = true;
        extraGroups =
          [ "wheel" "network" "plugdev" "adbusers" "vboxusers" "openrazer" ];
        hashedPassword =
          "$6$DMQjZ0Nn8JAb$2MBYjRZvhACwUJrDXI6GciNglr.KM3Yaza4CMUaG8HCxOJ2EtRqZZKvTBzRhIPQWjKiYeU3cCpntQNkToiUeu0";
      };
    };
  };

  # Recommended for pipewire
  security.rtkit.enable = true;

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    openrazer = {
      enable = true;
      devicesOffOnScreensaver = false;
      syncEffectsEnabled = true;
      mouseBatteryNotifier = true;
    };
  };

  documentation = {
    man.enable = true;
    info.enable = true;
  };

  programs = {
    fish.enable = true;
    adb.enable = true;
    gnupg.agent.enable = true;
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "JetBrainsMono" "FantasqueSansMono" ]; })
      iosevka
      comfortaa
      source-code-pro
      emacs-all-the-icons-fonts
      liberation_ttf
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
    ];

    fontconfig = {
      defaultFonts = {
        serif = [ "Cantarell" "Noto Kufi Arabic" ];
        sansSerif = [ "Cantarell" "Noto Kufi Arabic" ];
        monospace = [ "Cantarell" "Noto Kufi Arabic" ];
      };
    };
  };

  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
      gtkUsePortal = true;
    };
  };

  systemd.services = {
    systemd-resolved.enable = true;
    systemd-machined.enable = false;
  };

  services = {
    openssh = {
      enable = true;
      openFirewall = lib.mkDefault false;
    };

    upower.enable = true;
    printing.enable = true;

    xserver = {
      enable = true;
      layout = "us";

      libinput = {
        enable = true;
        touchpad = {
          naturalScrolling = true;
          tapping = true;
          disableWhileTyping = true;
        };
      };
    };

    flatpak.enable = true;

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };

      pulse.enable = true;

      # If you want to use JACK applications, uncomment:
      # jack.enable = true;

      # Bluetooth pipewire settings:
      media-session.config.bluez-monitor.rules = [
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
    };
  };

}
