{ config
, lib
, pkgs
, modulesPath
, ...
}: {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/home";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
    options = [ "x-gvfs-hide" ];
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  boot = {
    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ ];
    };
    extraModulePackages = [ ];
    kernelModules = [ "kvm-amd" ];
    kernelParams = [ "acpi_backlight=native" ];
    kernel.sysctl = {
      "net.ipv4.icmp_echo_ignore_broadcasts" = 1; # Refuse ICMP echo requests
    };
  };

  nix.settings.max-jobs = lib.mkDefault 4;

  hardware.cpu.amd = {
    updateMicrocode = true;
    # updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };

  powerManagement = {
    cpuFreqGovernor = "schedutil";
  };
}
