{ pkgs
, config
, lib
, ...
}: {
  imports = [ ./hwCfg.nix ];

  modules = {
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      input.enable = true;
      # kmonad = {
      #   enable = true;
      #   deviceID = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      # };
      xkbLayout.hyperCtrl.enable = true;
      razer.enable = true;
    };

    networking = {
      enable = true;
      networkManager.enable = true;
      wireGuard = {
        enable = true;
        akkadianVPN.enable = true;
      };
    };

    themes.active = "tokyonight";

    desktop = {
      qtile.enable = true;
      terminal = {
        default = "wezterm";
        wezterm.enable = true;
      };
      editors = {
        default = "nvim";
        helix.enable = true;
        neovim = {
          agasaya.enable = true;
          # ereshkigal.enable = true;
        };
      };
      browsers = {
        default = "firefox-devedition";
        unGoogled.enable = true;
        firefox.enable = true;
      };
      education = {
        memory.enable = true;
        vidcom.enable = true;
      };
      toolset = {
        downloader.transmission.enable = true;
        editor = {
          raster.enable = true;
          vector.enable = true;
        };
        player = {
          music.enable = true;
          video.enable = true;
        };
        social.common.enable = true;
        docView.sioyek.enable = true;
      };
      # virtual = {
      #   wine.enable = true;
      # };
    };

    develop = {
      haskell.enable = true;
      python.enable = true;
      rust.enable = true;
    };

    containers.transmission = {
      enable = false; # TODO: Once fixed -> enable = true;
      username = "alonzo";
      #  password = builtins.readFile config.age.secrets.torBylon.path;
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
      videoDrivers = [ "amdgpu" ];
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
