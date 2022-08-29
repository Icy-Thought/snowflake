{ pkgs
, config
, lib
, ...
}: {
  imports = [ ./hwCfg.nix ];

  modules = {
    hardware = {
      audio.enable = true;
      input.enable = true;
      # razer.enable = true;
    };

    networking = {
      enable = true;
      networkManager.enable = true;
      wireGuard = {
        enable = true;
        akkadianVPN.enable = true;
      };
    };

    themes.active = "one-dark";

    desktop = {
      gnome.enable = true;
      terminal = {
        default = "kitty";
        kitty.enable = true;
      };
      editors = {
        default = "nvim";
        neovim.enable = true;
      };
      browsers = {
        default = "brave";
        brave.enable = true;
      };
      media = {
        player = {
          video.enable = true;
        };
        document = {
          zathura.enable = true;
        };
      };
    };
  };

  services = {
    kdeconnect.enable = true;
  };

  shell = {
    git.enable = true;
    fish.enable = true;
    gnupg.enable = true;
  };

  services = {
    upower.enable = true;
    printing.enable = true;

    xserver = {
      videoDrivers = [ "modesetting" ];
      useGlamor = true;
    };
  };
}
