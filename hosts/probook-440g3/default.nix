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

  fileSystems."/".options = [ "noatime, x-gvfs-hide" ];
  fileSystems."/boot".options = [ "x-gvfs-hide" ];
  fileSystems."/home".options = [ "noatime, x-gvfs-hide" ];

  hardware.cpu.intel = { updateMicrocode = true; };
  hardware.opengl.extraPackages = with pkgs; [
    intel-compute-runtime
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
  ];

  i18n.defaultLocale = "en_US.UTF-8";
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;
  time.timeZone = "Europe/Berlin";

  user.name = "orca";
  networking.hostName = "ProBook-NixOS";

  services.xserver.videoDrivers = [ "modesetting" ];
  services.xserver.useGlamor = true;
}
