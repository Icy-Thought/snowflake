{ config, lib, pkgs, modulesPath, ... }:

let inherit (lib) mkDefault;
in {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    options = [ "noatime" "x-gvfs-hide" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/home";
    fsType = "ext4";
    neededForBoot = true;
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
    kernelModules = [ "thinkpad_acpi" "acpi_call" "kvm_amd" ];
    kernelParams = [ ];
    kernel.sysctl = {
      "net.ipv4.icmp_echo_ignore_broadcasts" = 1; # Refuse ICMP echo requests
    };
  };

  nix.settings.max-jobs = mkDefault 4;

  hardware.cpu.amd = {
    updateMicrocode = true;
    # updateMicrocode = mkDefault config.hardware.enableRedistributableFirmware;
  };

  # TLP-specific:
  services.tlp.settings = {
    # USB_DENYLIST = "04:00.3 04:00.4";
    USB_EXCLUDE_BTUSB = 1;
  };

  # Finally, our beloved hardware module(s):
  modules.hardware = {
    powerCtrl.enable = true;
    pipewire.enable = true;
    bluetooth.enable = true;
    kmonad.deviceID = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
    pointer.enable = true;
    razer.enable = true;
  };
}
