{ config, lib, pkgs, ... }: {

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
      cc.enable = true;
      python.enable = true;
      rust.enable = true;
      haskell.enable = true;
      scientific.typst.enable = true;
    };

    themes.active = "kanagawa";
    desktop = {
      xmonad.enable = true;
      terminal = {
        default = "alacritty";
        alacritty.enable = true;
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
          matrix.withDaemon.enable = true;
          mu4e.enable = true;
        };
        readers = {
          enable = true;
          program = "zathura";
        };
      };
      distractions.steam.enable = true;
    };
    virtualize.enable = true;
  };

  # KDE-Connect + Start-up indicator
  programs.kdeconnect = {
    enable = true;
    # package = pkgs.valent;
  };
}
