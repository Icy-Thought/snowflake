{ pkgs, config, lib, ... }: {

  imports = [ ./hwCfg.nix ];

  modules.hardware = {
    audio.enable = true;
    bluetooth.enable = true;
    touchpad.enable = true;
    openrazer.enable = true;
  };

  modules.networking = {
    enable = true;
    networkManager.enable = true;

    wireGuard = {
      enable = true;
      akkadianVPN.enable = true;
    };
  };

  modules.themes = { active = "catppuccin"; };

  modules.desktop = {
    xmonad.enable = true;
    terminal = {
      default = "alacritty";
      alacritty.enable = true;
    };
    editors = {
      default = "nvim";
      emacs.enable = true;
      nvim.enable = true;
      vscodium.enable = true;
    };
    browsers = {
      default = "firefox";
      firefox.enable = true;
      unGoogled.enable = true;
    };
    philomath.aula = {
      anki.enable = true;
      zoom.enable = true;
    };
    media = {
      mpv.enable = true;
      spotify.enable = true;
      graphics.enable = true;
      docViewer.enable = true;
      transmission.enable = true;
      chat = {
        enable = true;
        mobile.enable = true;
      };
    };
    gaming = { steam.enable = false; };
  };

  modules.develop = {
    haskell.enable = true;
    node.enable = true;
    python.enable = true;
    rust.enable = true;
  };

  modules.containers.transmission = {
    enable = false; # TODO: Once fixed -> enable = true;
    username = "alonzo";
    password = builtins.readFile config.age.secrets.torBylon.path;
  };

  modules.services = {
    # ssh.enable = true;
    laptop.enable = true;
    kdeconnect.enable = true;
  };

  modules.shell = {
    git.enable = true;
    fish.enable = true;
    gnupg.enable = true;
  };

  boot.kernel.sysctl."abi.vsyscall32" = 0; # League of Legends..
  boot.kernelParams = [ "acpi_backlight=native" ];

  hardware.opengl.extraPackages =
    [ pkgs.amdvlk pkgs.driversi686Linux.amdvlk pkgs.rocm-opencl-icd ];

  systemd.services.systemd-udev-settle.enable = false;

  services = {
    avahi.enable = false;
    gvfs.enable = true;
  };

  services.xserver = {
    videoDrivers = [ "amdgpu" ];
    deviceSection = ''
      Option "TearFree" "true"
    '';
  };

  services.xserver.libinput = {
    touchpad.accelSpeed = "0.5";
    touchpad.accelProfile = "adaptive";
  };
}
