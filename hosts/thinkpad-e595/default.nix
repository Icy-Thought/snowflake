{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [./hardware.nix];

  modules = {
    shell = {
      default = "zsh";
      toolset = {
        git.enable = true;
        gnupg.enable = true;
        android.enable = true;
      };
    };
    networking.networkManager.enable = true;

    services = {
      ssh.enable = true;
      rustdesk.enable = true;
    };

    develop = {
      python.enable = true;
      rust.enable = true;
      scientific.typst.enable = true;
    };

    themes.active = "catppuccin";
    desktop = {
      xmonad.enable = true;
      terminal = {
        default = "wezterm";
        wezterm.enable = true;
      };
      editors = {
        default = "emacsclient";
        emacs.enable = true;
        neovim.enable = true;
      };
      browsers = {
        default = "firefox";
        firefox = {
          enable = true;
          privacy.enable = true;
        };
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
      distraction.steam.enable = true;
    };
  };

  # KDE-Connect + Start-up indicator
  programs.kdeconnect = {
    enable = true;
    package = pkgs.valent;
  };
}
