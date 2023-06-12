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
    networking = {
      networkManager.enable = true;
      mullvad.enable = true;
      # samba.sharing.enable = true;
    };
    services = {ssh.enable = true;};
    containers.transmission = {
      enable = false; # TODO: Once fixed -> enable = true;
      username = "alonzo";
      #  password = builtins.readFile config.age.secrets.torBylon.path;
    };
    develop = {
      haskell.enable = true;
      python.enable = true;
      rust.enable = true;
      scientific = {
        latex.enable = true;
        typst.enable = true;
      };
    };
    themes.active = "rose-pine";

    desktop = {
      xmonad.enable = true;
      terminal = {
        default = "alacritty";
        alacritty.enable = true;
      };
      editors = {
        default = "nvim";
        neovim.enable = true;
        emacs = {
          enable = true;
          transparency.enable = true;
        };
      };
      browsers = {
        default = "firefox";
        ungoogled.enable = true;
        firefox.enable = true;
      };
      education = {
        memory.enable = true;
        vidcom.enable = true;
      };
      toolset = {
        downloader.transmission.enable = true;
        graphics = {
          # raster.enable = true;
          vector.enable = true;
        };
        player = {
          music.enable = true;
          video.enable = true;
        };
        social = {
          base.enable = true;
          element.withClient.enable = true;
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
  programs.kdeconnect.enable = true;
  systemd.user.services.kdeconnect-indicator = {
    serviceConfig.ExecStart = "${pkgs.plasma5Packages.kdeconnect-kde}/bin/kdeconnect-indicator";
    wantedBy = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
  };
}
