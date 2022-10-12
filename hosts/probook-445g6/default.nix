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
    };

    networking = {
      enable = true;
      networkManager.enable = true;
      wireGuard.enable = true;
    };

    themes.active = "catppuccin";

    desktop = {
      gnome.enable = true;
      terminal = {
        default = "wezterm";
        wezterm.enable = true;
      };
      editors = {
        default = "nvim";
        neovim.agasaya.enable = true;
      };
      browsers = {
        default = "firefox-devedition";
        firefox.enable = true;
      };
      extensions.player = {
        video.enable = true;
      };
      toolset.viewer = {
        zathura.enable = true;
      };
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
    xserver.videoDrivers = [ "modesetting" ];
  };
}
