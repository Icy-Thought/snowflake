{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../../hardware/ProBook-440G3.nix
      ./services.nix
      ./users.nix
      ./packages.nix
    ];

  # Build NixOS from latest stable release.
  system.stateVersion = "21.05"; # Did you read the comment?

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };


  nix = {
    package = pkgs.nixFlakes;

    # Automate `nix-store --optimise`
    autoOptimiseStore = true;

    extraOptions = lib.optionalString (config.nix.package == pkgs.nixFlakes)
    ''
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
    kernelParams = [ "pcie_aspm.policy=performance" ];
    
    # Set GRUB2 to default boot.
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };

      grub = {
        enable = true;
        version = 2;
        efiSupport = true;
        device = "nodev";
      };
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/6a76fd5f-b327-43fb-81cd-aef0c69deb7a";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/9C99-45AA";
    fsType = "vfat";
    options = [ "x-gvfs-hide" ];
  };

  # Network configurations.
  networking = {
    hostName = "NixOS";

    # Global useDHCP => deprecated.
    useDHCP = false;

    # Per-interface useDHCP is mandatory. (Not Required by NetworkManager)
    # interfaces = {
    #   enp1s0.useDHCP = true;
    #   wlan0.useDHCP = true;
    # };

    networkmanager = {
      enable = true;
      # wifi.backend = "iwd";
    };
  };

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ 
        fcitx5-gtk
        fcitx5-configtool
        fcitx5-chinese-addons
        fcitx5-mozc
        # fcitx5-hangul
      ];
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "Europe/Stockholm";

  # Recommended for pipewire
  security = {
    rtkit = {
      enable = true;
    };
  };

  hardware = {
    opengl = {
      enable = true;

      extraPackages = with pkgs; [
        intel-compute-runtime
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
      ];

      driSupport = true;
      driSupport32Bit = true;
    };

    pulseaudio = {
      enable = false;
    };

    bluetooth = {
      enable = true;
    };

    openrazer = {
      enable = true;
      devicesOffOnScreensaver = false;
      syncEffectsEnabled = true;
      mouseBatteryNotifier = true;
    };

  };

  virtualisation = {
    podman = {
      enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;
    };
  };

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

  documentation = {
    man.enable   = true;
    info.enable  = true;
  };

  programs = {
    fish.enable  = true;
    adb.enable   = true;
  };

}
