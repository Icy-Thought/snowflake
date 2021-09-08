{ config, pkgs, lib, ... }:

let
  imports = [
    ../modules/common.nix
    ../modules/nixos/fcitx5.nix
    ../modules/desktop-managers/plasma.nix
    # ../modules/window-managers/xmonad.nix
  ];

in {
  inherit imports;

  hm = { imports = [ ./home/ProBook-440G3.nix ]; };

  boot = { kernelParams = [ "pcie_aspm.policy=performance" ]; };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/42c5c3e5-38df-4007-9fff-5c9841c93a0a";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/e688ecea-6ebd-4740-bd65-6bc27ae2c0db";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/097C-54E2";
    fsType = "vfat";
    options = [ "x-gvfs-hide" ];
  };

  hardware = {
    cpu.intel = { updateMicrocode = true; };

    opengl.extraPackages = with pkgs; [
      intel-compute-runtime
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  user.name = "orca";
  networking.hostName = "ProBook-NixOS";

  services = {
    xserver = {
      videoDrivers = [ "modesetting" ];
      useGlamor = true;
    };
  };

}
