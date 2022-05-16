{
  modulesPath,
  pkgs,
  config,
  ...
}: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
  ];

  # In case of proprietary wireless drivers
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_5_16;
    kernelModules = ["wl"];
    extraModulePackages = [config.boot.kernelPackages.broadcom_sta];
  };

  environment.systemPackages = with pkgs; [
    fish
    git
    nixFlakes
  ];
}
# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=./default.nix

