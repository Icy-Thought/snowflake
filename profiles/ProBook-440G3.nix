{ config, pkgs, lib, ... }:

{
  hm = { imports = [ ./home-manager/ProBook-440G3.nix ]; };
  imports = [ ../modules/common.nix ];

  # Build NixOS from latest stable release.
  system.stateVersion = "21.05"; # Did you read the comment?

  # Boot configurations.
  boot = {
    kernelParams = [ "pcie_aspm.policy=performance" ];
  };

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

  networking.hostName = "probook-nixos";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ 
        fcitx5-gtk
        fcitx5-configtool
        fcitx5-chinese-addons
        fcitx5-mozc
        # fcitx5-hangul
      ];
    };
  };

  hardware = {
    cpu.intel = {
      updateMicrocode = true;
    };

    opengl.extraPackages = with pkgs; [
      intel-compute-runtime
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  services = {
    xserver = {
      videoDrivers = [ "modesetting" ];
      useGlamor = true;

      displayManager.sddm = {
        enable = true;
      };

      desktopManager.plasma5 = {
        enable = true;
      };
    };
  };

}
