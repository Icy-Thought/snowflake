{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [./hwCfg.nix];

  modules.hardware = {
    audio.enable = true;
    bluetooth.enable = true;
    razer.enable = true;
    touchpad.enable = true;
  };

  modules.networking = {
    enable = true;
    networkManager.enable = true;

    wireGuard = {
      enable = true;
      akkadianVPN.enable = true;
    };
  };

  modules.themes = {
    active = "catppuccin";
  };

  modules.desktop = {
    xmonad.enable = true;
    terminal = {
      default = "alacritty";
      alacritty.enable = true;
    };
    editors = {
      default = "nvim";
      emacs.enable = true;
      neovim = {
        enable = true;
        niflheim.enable = true;
      };
    };
    browsers = {
      default = "brave";
      brave.enable = true;
      firefox.enable = true;
    };
    philomath.aula = {
      anki.enable = true;
      zoom.enable = true;
    };
    media = {
      downloader = {
        transmission.enable = true;
      };
      editor = {
        raster.enable = true;
        vector.enable = true;
      };
      social = {
        common.enable = true;
      };
      viewer = {
        video.enable = true;
        music.enable = true;
        document.enable = true;
      };
    };
    virtual.wine.enable = true;
  };

  modules.develop = {
    haskell.enable = true;
    julia.enable = true;
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
    kdeconnect.enable = true;
  };

  modules.shell = {
    git.enable = true;
    fish.enable = true;
    gnupg.enable = true;
  };

  boot = {
    kernel.sysctl."abi.vsyscall32" = 0; # League of Legends..
    kernelParams = ["acpi_backlight=native"];
  };

  hardware.opengl.extraPackages = with pkgs; [
    amdvlk
    driversi686Linux.amdvlk
    rocm-opencl-icd
  ];

  services = {
    upower.enable = true;
    printing.enable = true;

    xserver = {
      videoDrivers = ["amdgpu"];
      deviceSection = ''
        Option "TearFree" "true"
      '';
    };
  };

  services.xserver.libinput.touchpad = {
    accelSpeed = "0.5";
    accelProfile = "adaptive";
  };
}
