{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf;
  cfg = config.modules.virtualize;
in {
  options.modules.virtualize = let
    inherit (lib.options) mkEnableOption;
  in {
    enable = mkEnableOption "Spawn virtual envionrments where required.";
  };

  config = mkIf cfg.enable {
    user.packages = [pkgs.virt-manager];

    virtualisation.libvirtd = {
      enable = true;
      # extraOptions = ["--verbose"];
      qemu.runAsRoot = false;
    };
    user.extraGroups = ["libvirtd"];

    # Fix: Could not detect a default hypervisor. Make sure the appropriate QEMU/KVM virtualization...
    hm.dconf.settings = {
      "org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    };
  };
}
