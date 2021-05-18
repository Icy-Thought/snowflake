{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hw-config/ProBook.nix
    ];

  # Build NixOS from latest stable release.
  system.stateVersion = "20.09"; # Did you read the comment?

  nix = {
    package = pkgs.nixUnstable;

    # Automate `nix-store --optimise`
    autoOptimiseStore = true;

    extraOptions = ''
      experimental-features = nix-command flakes

      # Avoid unwanted garbage collection when using nix-direnv
      keep-outputs     = true
      keep-derivations = true
    '';

    # Automate garbage collection
    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 7d";
    };

    # Required by Cachix to be used as non-root user
    trustedUsers = [ "root" "sirius" ];

  };

  # Boot configurations.
  boot = {
    kernelPackages = pkgs.linuxPackages_xanmod;
    # kernelPackages = pkgs.linuxPackages_xanmod.amdgpu-pro;
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
      };
    };
  };

  # Network configurations.
  networking = {
    hostName = "NixOS";
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    useDHCP = false;

    interfaces = {
      enp1s0.useDHCP = true;
    };

    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "Europe/Stockholm";

  fonts.fonts = with pkgs; [
    source-code-pro
    emacs-all-the-icons-fonts
    iosevka-bin
    liberation_ttf
    font-awesome
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
  ];

  # Recommended for pipewire
  security = {
    rtkit = {
      enable = true;
    };
  };

  hardware = {
    opengl = {
      driSupport = true;
      driSupport32Bit = true;
    };

    pulseaudio = {
      enable = false;
    };

    bluetooth = {
      enable = true;
    };
  };

  # Enable X11 + CUPS + Flatpak.
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
    
      # GNOME DE:
      displayManager.gdm = {
        enable  = true;
        wayland = true;
      };

      desktopManager.gnome = {
        enable = true;
      };

      # KDE-Plasma
      # displayManager.sddm.enable = true;
      # desktopManager.plasma5.enable = true;
    };

    dbus.packages = [ pkgs.gnome.dconf ];
    udev.packages = [ pkgs.gnome.gnome-settings-daemon ];

    mpd = {
      enable = true;
      extraConfig = builtins.readFile ../../nixpkgs/config/mpd.conf;
    };

  };

  environment.systemPackages = with pkgs; [
    wayland                                             # Wayland window system code + protocol.
    mesa                                                # FOSS 3D Graphics Lib.
    vulkan-headers                                      # Vulkan Header files + API registery.
    fish                                                # Shell with better defaults.
    iwd                                                 # WPA_Supplicant alternative.
    pipewire                                            # Multimedia pipeline API.
    git                                                 # Tool for git usage.
    latest.firefox-beta-bin                             # Firefox + dev-tools enabled.
  ];

  # fileSystems = { //fix
  #   "/".options = [ "noatime,x-gvfs-hide" ];
  #   "/boot/".options = [ "noatime,x-gvfs-hide" ];
  # };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = pkgs.bash;
    mutableUsers = false;

    users.root = {
       initialHashedPassword = "$6$DMQjZ0Nn8JAb$2MBYjRZvhACwUJrDXI6GciNglr.KM3Yaza4CMUaG8HCxOJ2EtRqZZKvTBzRhIPQWjKiYeU3cCpntQNkToiUeu0";
       shell = pkgs.fish;
       packages = with pkgs; [ (neovim.override { viAlias = true; vimAlias = true; withNodeJs = true;}) ];
    };

    users.sirius = {
      initialHashedPassword = "$6$DMQjZ0Nn8JAb$2MBYjRZvhACwUJrDXI6GciNglr.KM3Yaza4CMUaG8HCxOJ2EtRqZZKvTBzRhIPQWjKiYeU3cCpntQNkToiUeu0";
      extraGroups = [ "wheel" "users" "network" "audio" "video" "storage" "plugdev" "adbusers" ];
      shell = pkgs.fish;
      isNormalUser = true;
    };

    users.orca = {
      initialHashedPassword = "$6$Xny1A0ZwSSw/t1$3MUaZ0Cr4nV/N.n2VTWLIg1of8SAzAFm7EA.KRFYXeRRitIfKAAeFLT8AVGxP8NyhYOPkRngclRQjqc5Gmzqb0";
      extraGroups = [ "wheel" "users" "network" "audio" "video" "storage" "plugdev" "adbusers" ];
      shell = pkgs.fish;
      isNormalUser = true;
    };
  };
}
