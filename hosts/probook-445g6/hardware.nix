{ config, lib, pkgs, modulesPath, ... }:

let inherit (lib) mkDefault attrValues;
in {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    options = [ "noatime" "x-gvfs-hide" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
    options = [ "x-gvfs-hide" ];
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  boot = {
    initrd = {
      availableKernelModules =
        [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ ];
    };
    extraModulePackages = [ ];
    kernelModules = [ "kvm-amd" ];
    kernelParams = [ ];
    kernel.sysctl = {
      "net.ipv4.icmp_echo_ignore_broadcasts" = 1; # Refuse ICMP echo requests
    };
  };

  nix.settings.max-jobs = mkDefault 4;

  hardware.cpu.amd = {
    updateMicrocode = mkDefault config.hardware.enableRedistributableFirmware;
  };

  # Manage device power-control:
  services = {
    power-profiles-daemon.enable = false;
    thermald.enable = false;
    tlp = {
      enable = true;
      settings = {
        START_CHARGE_THRESH_BAT0 = 75;
        STOP_CHARGE_THRESH_BAT0 = 80;
        RESTORE_THRESHOLDS_ON_BAT = 1;

        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";

        PCIE_ASPM_ON_AC = "performance";
        PCIE_ASPM_ON_BAT = "powersave";

        # Prevents bluez from hanging:
        USB_EXCLUDE_BTUSB = 1;
      };
    };
  };

  # Finally, call-forward our beloved module(s):
  modules.hardware = {
    pipewire.enable = true;
    bluetooth.enable = true;
    pointer.enable = true;
  };

  services = {
    upower.enable = true;
    xserver.videoDrivers = [ "modesetting" ];
  };
}
