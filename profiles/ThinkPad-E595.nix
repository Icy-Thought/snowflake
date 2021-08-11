{ config, pkgs, lib, ... }: {

  imports = [
    ../modules/common.nix
    ../modules/nixos/fcitx5.nix
    # ../modules/desktop-managers/gnome.nix
    ../modules/window-managers/xmonad.nix
  ];

  hm = { imports = [ ./home/ThinkPad-E595.nix ]; };

  boot = {
    # extraModulePackages = with config.boot.kernelPackages; [ amdgpu-pro ];
    kernel.sysctl."abi.vsyscall32" = 0; 
    kernelParams = [ "acpi_backlight=native" ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/a2ee4473-ef03-4cb9-8103-ba4c3d8afb1e";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/3988-91C5";
    fsType = "vfat";
    options = [ "x-gvfs-hide" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/4b1d85cf-c670-4e7e-9b4a-02b3657338dd";
    fsType = "ext4";
  };

  hardware = {
    cpu.amd = { updateMicrocode = true; };

    opengl.extraPackages =
      [ pkgs.amdvlk pkgs.driversi686Linux.amdvlk pkgs.rocm-opencl-icd ];
  };

  user.name = "sirius";
  networking.hostName = "ThinkPad-NixOS";

  virtualisation = {
    virtualbox.host.enable = true;

    podman = {
      enable = false;
      dockerCompat = true;
    };
  };

  environment.systemPackages =
    [ (pkgs.steam.override { nativeOnly = true; }).run ];

  programs.steam.enable = true;

  environment.variables = {
    VK_ICD_FILENAMES =
      [ "/run/opengl-driver/share/vulkan/icd.d/amd_icd64.json" ];
  };

  systemd.services = {
    NetworkManager-wait-online.enable = false;
    systemd-udev-settle.enable = false;
  };

  services = {
    avahi.enable = false;
    gvfs.enable = true;

    xserver = {
      videoDrivers = [ "amdgpu" ];
      deviceSection = ''
        Option "TearFree" "true"
      '';
    };
  };
}
