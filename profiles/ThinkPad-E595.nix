{ config, pkgs, lib, ... }:

{
  imports = [ 
    ../modules/common.nix
  ];

  hm = { imports = [ ./home-manager/ThinkPad-E595.nix ]; };

  # Build NixOS from latest stable release.
  system.stateVersion = "21.05"; # Did you read the comment?

  boot = {
    extraModulePackages = with config.boot.kernelPackages; [ 
      # amdgpu-pro
    ];
    
    kernelParams = [
      "acpi_backlight=native"
    ];
  };
    
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/a2ee4473-ef03-4cb9-8103-ba4c3d8afb1e";
      fsType = "ext4";
      options = [ "noatime, x-gvfs-hide" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/3988-91C5";
      fsType = "vfat";
      options = [ "x-gvfs-hide" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/4b1d85cf-c670-4e7e-9b4a-02b3657338dd";
      fsType = "ext4";
    };

  hardware = {
    cpu.amd = {
      updateMicrocode = true;
    };

    opengl.extraPackages = with pkgs; [
      amdvlk
      driversi686Linux.amdvlk
      rocm-opencl-icd
    ];

  };

  user.name = "sirius";
  networking.hostName = "ThinkPad-NixOS";

  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ 
      libpinyin 
      hangul 
      mozc 
    ];
  };

  virtualisation = {
    podman = {
      enable = false;
      dockerCompat = true;
    };
  };
  
  programs = {
    dconf.enable = true;
  };

  environment.variables = {
    VK_ICD_FILENAMES = [ "/run/opengl-driver/share/vulkan/icd.d/amd_icd64.json" ];
  };

  services = {
    avahi.enable = false;
    gvfs.enable = true;

    gnome = { # Remove after fixing gnome/default.nix
      gnome-keyring.enable = true;
      chrome-gnome-shell.enable = true;
    };

    xserver = {
      videoDrivers = [ "amdgpu"];

      displayManager.gdm = {
        enable  = true;
        wayland = true;
      };

      desktopManager.gnome = {
        enable = true;
      };
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

  };

}
