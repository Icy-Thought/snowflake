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
    services.ssh.enable = true;

    develop = {
      haskell.enable = true;
      python.enable = true;
      rust.enable = true;
      scientific = {
        latex.enable = true;
        typst.enable = true;
      };
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
        helix.enable = true;
        neovim.enable = true;
      };
      browsers = {
        default = "firefox";
        firefox = {
          enable = true;
          privacy.enable = true;
        };
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
        social = {
          base.enable = true;
          matrix.withDaemon.enable = true;
        };
        # docView.sioyek.enable = true;
        docViewer = {
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
