{ config, pkgs, lib, ... }:
{

  
  virtualisation = {
    libvirtd = {
      enable = true;
      qemuVerbatimConfig = ''
        user = "sirius"
      '';
    }
  };

  systemd.nspawn."Arch" = {
    enable = true;
    wantedBy = [ "machines.target" ];
    requiredBy = [ "machines.target" ];
    execConfig = {
      Timezone = "Bind";
      Hostname = "Arch";
      SystemCallFilter = "modify_ldt";
    };
    filesConfig = {
      Bind = [ "/home/sirius/.container-arch:/home/sirius" "/run/user/1000/wayland-1" "/tmp/.X11-unix/X0" "/tank" "/run/user/1000/pulse/native" "/dev/dri" "/dev/shm" ];
      BindReadOnly = [ "/home/sirius:/mnt/sirius" ];
      Volatile = false;
    };
    networkConfig = {
      Private = true;
      VirtualEthernet = true;
      Bridge = "virbr0";
    };
  };

  systemd.services."systemd-nspawn@".serviceConfig = {
    # Vulkan support
    DeviceAllow = [
      "char-drm rwx"
      "/dev/dri/renderD128"
    ];
  };
}
