{ pkgs, config, lib, ... }: {

  imports = [ ./hwCfg.nix ];

  modules.hardware = {
    pipewire.enable = true;
    touchpad.enable = true;
    # openrazer.enable = true;
  };

  modules.networking = {
    enable = true;
    networkManager.enable = true;

    wireGuard = {
      enable = true;
      akkadianVPN.enable = true;
    };
  };

  modules.themes = { active = "one-dark"; };

  modules.desktop = {
    gnome.enable = true;

    appliances = {
      termEmu = {
        default = "alacritty";
        alacritty.enable = true;
      };
      editors = {
        default = "emacs";
        emacs.enable = true;
        # nvim.enable = true;
      };
      browsers = {
        default = "firefox";
        firefox.enable = true;
        unGoogled.enable = true;
      };
      extras = {
        chat.enable = true;
        docViewer.enable = true;
      };
      media = {
        mpv.enable = true;
        spotify.enable = true;
        # graphics.enable = true;
      };
    };
  };

  modules.services = {
    kdeconnect.enable = true;
    laptop.enable = true;
  };

  modules.shell = {
    git.enable = true;
    fish.enable = true;
    gnupg.enable = true;
  };

  # Hide device entry from file-manager:
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/home";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
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

  services.xserver = {
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
  };
}
