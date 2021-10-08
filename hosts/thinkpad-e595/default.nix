{ config, pkgs, lib, ... }:

with lib;
with lib.my;

let imports = [ ../modules/common.nix ../modules/window-managers/xmonad.nix ];

in {
  inherit imports;

  boot.kernel.sysctl."abi.vsyscall32" = 0; # League of Legends..
  boot.kernelParams = [ "acpi_backlight=native" ];

  # Remove device entry from file-manager:
  fileSystems."/".device = "/dev/disk/by-label/nixos";
  filesystems."/".options = [ "noatime, x-gvfs-hide" ];

  fileSystems."/boot".device = "/dev/disk/by-label/boot";
  fileSystems."/boot".options = [ "x-gvfs-hide" ];

  fileSystems."/home".device = "/dev/disk/by-label/home";
  fileSystems."/home".options = [ "noatime, x-gvfs-hide" ];

  hardware.cpu.amd.updateMicrocode = true;
  hardware.opengl.extraPackages =
    [ pkgs.amdvlk pkgs.driversi686Linux.amdvlk pkgs.rocm-opencl-icd ];

  i18n.defaultLocale = "en_US.UTF-8";
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;
  time.timeZone = "Europe/Berlin";

  user.name = "sirius";
  user.extraGroups = [ "vboxusers" ];
  networking.hostName = "ThinkPad-NixOS";

  systemd.services = {
    NetworkManager-wait-online.enable = false;
    systemd-udev-settle.enable = false;
  };

  services.avahi.enable = false;
  services.gvfs.enable = true;

  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  services.libinput.touchpad.accelSpeed = "0.5";
  services.libinput.touchpad.accelProfile = "adaptive";
}
