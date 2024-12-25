{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
  virtCfg = config.modules.virtualisation;
in {
  options.modules.virtualisation.kvm = let inherit (lib.options) mkEnableOption;
  in {
    enable = mkEnableOption "Kernel-based Virutal Machine.";
    vendor = lib.types.enum [ "amd" "intel" ] "amd";
  };

  config = mkIf virtCfg.kvm.enable {
    modules.virtualisation.looking-glass.enable = true;

    user.packages = attrValues { inherit (pkgs) virt-manager; };

    boot = {
      kernelModules =
        # https://wiki.archlinux.org/title/PCI_passthrough_via_OVMF#modprobe.d
        [ "kvm-${virtCfg.vendor}" "vfio" "vfio_iommu_type1" "vfio_pci" ];
      kernelParams = [
        "${virtCfg.vendor}_iommu=on"
        "${virtCfg.vendor}_iommu=pt"
        "kvm.ignore_msrs=1" # https://pve.proxmox.com/wiki/PCI_Passthrough#Tips
      ];
      extraModprobeConfig = ''
        softdep drm pre: vfio vfio-pci
      '';
    };

    virtualisation = {
      libvirtd = {
        enable = true;
        onBoot = "ignore";
        onShutdown = "shutdown";
        qemu = {
          ovmf = true;
          swtpm = true;
          runAsRoot = false;
        };
      };
    };

    user.extraGroups = [ "libvirtd" ];

    systemd.user.services.scream-ivshmem = {
      after =
        [ "libvirtd.service" "pipewire.service" "pipewire-pulse.service" ];
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        Restart = "on-failure";
        ExecStart = "${lib.getExe pkgs.scream} -m /dev/shm/scream -o pulse";
      };
    };
  };
}