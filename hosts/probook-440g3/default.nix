{ pkgs, config, lib, ... }: {

  imports = [ ./hwCfg.nix ];

  # Hardware-related Modules:
  modules.hardware = {
    audio.enable = true;
    # openrazer.enable = true;
  };

  # Networking-related Modules:
  modules.networking = {
    enable = true;
    networkManager.enable = true;
    wireGuard.enable = true;
    wireGuard.akkadianVPN.enable = true;
  };

  modules.desktop = {
    envDisplay.sddm.enable = true;
    envManager.plasma.enable = true;
    inputMF = { spellCheck.enable = true; };
  };

  modules.fonts.entry.enable = true;

  modules.themes.active = "one-dark";

  modules.appliances = {
    termEmu = {
      default = "alacritty";
      alacritty.enable = true;
    };

    # Editor-related Modules:
    editors = {
      default = "emacs";
      emacs.enable = true;
      # nvim.enable = true;
    };

    # Browser-related Modules:
    browsers = {
      default = "firefox";
      firefox.enable = true;
      unGoogled.enable = true;
    };

    # Random Application Modules:
    extras = {
      chat.enable = true;
      docViewer.enable = true;
    };

    # Media-related Modules:
    media = {
      mpv.enable = true;
      spotify.enable = true;
      # graphics.enable = true;
    };
  };

  # Services-related Modules:
  modules.services = {
    xserver.enable = true;
    xserver.touch.enable = true;

    kdeconnect.enable = true;
    laptop.enable = true;
  };

  # Shell-related Modules:
  modules.shell = {
    git.enable = true;
    gnupg.enable = true;
    bash.enable = true;
    fish.enable = true;
    starship.enable = true;
    htop.enable = true;
    neofetch.enable = true;
    printTermColor.enable = true;
  };

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

  networking.hostName = "ProBook-NixOS";

  services.xserver = {
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
  };
}
