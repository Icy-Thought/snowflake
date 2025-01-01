{ config, lib, pkgs, modulesPath, ... }:

{
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
    kernel.sysctl = {
      "net.ipv4.icmp_echo_ignore_broadcasts" = 1; # Refuse ICMP echo requests
    };
  };

  hardware.cpu.intel.updateMicrocode = true;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  nix.settings.max-jobs = lib.mkDefault 4;

  # Manage device power-control:
  services = {
    power-profiles-daemon.enable = true;
    thermald.enable = true;
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
