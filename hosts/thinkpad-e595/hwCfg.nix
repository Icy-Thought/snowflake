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
    options = [ "noatime" "x-gvfs-hide" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/home";
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
      availableKernelModules = [
        "ahci"
        "amdgpu"
        "rtsx_pci_sdmmc"
        "sd_mod"
        "usb_storage"
        "usbhid"
        "xhci_pci"
      ];
      kernelModules = [ ];
    };
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    kernelModules = [
      "thinkpad_acpi"
      "acpi_call"
      "kvm_amd"
    ];
    kernelParams = [ ];
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

  # Finally, our beloved hardware module(s):
  modules.hardware = {
    audio.enable = true;
    bluetooth.enable = true;
    kmonad.deviceID = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
    pointer.enable = true;
    razer.enable = true;
  };
}
