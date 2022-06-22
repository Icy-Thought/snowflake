{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [./hwCfg.nix];

  modules = {
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      razer.enable = true;
      touchpad.enable = true;
    };

    networking = {
      enable = true;
      networkManager.enable = true;
      wireGuard = {
        enable = true;
        akkadianVPN.enable = true;
      };
    };

    themes.active = "catppuccin";

    desktop = {
      qtile.enable = true;
      terminal = {
        default = "kitty";
        kitty.enable = true;
      };
      editors = {
        default = "nvim";
        neovim.agasaya.enable = true;
      };
      browsers = {
        default = "brave";
        brave.enable = true;
        firefox.enable = true;
      };
      philomath.aula = {
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

    develop = {
      haskell.enable = true;
      python.enable = true;
      rust.enable = true;
    };

    containers.transmission = {
      enable = false; # TODO: Once fixed -> enable = true;
      username = "alonzo";
      password = builtins.readFile config.age.secrets.torBylon.path;
    };

    services = {
      # ssh.enable = true;
      kdeconnect.enable = true;
    };

    shell = {
      git.enable = true;
      fish.enable = true;
      gnupg.enable = true;
    };
  };

  services = {
    upower.enable = true;
    printing.enable = true;

    xserver = {
      videoDrivers = ["amdgpu"];
      deviceSection = ''
        Option "TearFree" "true"
      '';
      libinput.touchpad = {
        accelSpeed = "0.5";
        accelProfile = "adaptive";
      };
    };
  };
}
