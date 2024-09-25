{ pkgs, config, lib, ... }: {

  imports = [ ./hardware.nix ];

  modules = {
    shell = {
      default = "zsh";
      toolset = {
        git.enable = true;
        android.enable = true;
      };
    };
    networking.networkManager.enable = true;
    services.ssh.enable = true;

    develop = {
      python.enable = true;
      rust.enable = true;
      haskell.enable = true;
    };

    themes.active = "catppuccin";
    desktop = {
      hyprland.enable = true;
      terminal = {
        default = "kitty";
        kitty.enable = true;
      };
      editors = {
        default = "emacsclient";
        emacs.enable = true;
        neovim.enable = true;
      };
      browsers = {
        default = "zen";
        zen.enable = true;
        # nyxt.enable = true;
      };
      education = {
        memorization.enable = true;
        vidcom.enable = false;
      };
      toolset = {
        graphics = {
          raster.enable = true;
          vector.enable = true;
        };
        player = {
          music.enable = true;
          video.enable = true;
        };
        recorder = {
          enable = true;
          video.enable = true;
        };
        communication = {
          base.enable = true;
          notmuch.enable = true;
          matrix.withDaemon.enable = true;
        };
        readers = {
          enable = true;
          program = "zathura";
        };
      };
    };
  };

  # KDE-Connect + Start-up indicator
  programs.kdeconnect = {
    enable = true;
    package = pkgs.valent;
  };
}
