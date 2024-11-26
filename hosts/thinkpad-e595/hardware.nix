{ config, lib, pkgs, modulesPath, ... }:

let inherit (lib.modules) mkDefault;
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
    extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
    kernelModules = [ "thinkpad_acpi" "acpi_call" "kvm_amd" ];
    kernelParams = [ "pcie_aspm.policy=performance" ];
    kernel.sysctl = {
      "net.ipv4.icmp_echo_ignore_broadcasts" = 1; # Refuse ICMP echo requests
    };
  };

  hardware.cpu.amd.updateMicrocode = true;
  powerManagement.cpuFreqGovernor = mkDefault "performance";
  nix.settings.max-jobs = mkDefault 4;

  # Manage device power-control:
  services.power-profiles-daemon.enable = true;

  # Finally, our beloved hardware module(s):
  modules.hardware = {
    pipewire.enable = true;
    bluetooth.enable = true;
    kmonad.deviceID = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
    pointer.enable = true;
    printer.enable = true;
    razer.enable = true;
  };

  services = {
    upower.enable = true;
    libinput.touchpad = {
      accelSpeed = "0.5";
      accelProfile = "adaptive";
    };
  };
}
