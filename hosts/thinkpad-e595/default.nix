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
    develop = {
      haskell.enable = true;
      python.enable = true;
      rust.enable = true;
      scientific = {
        latex.enable = true;
        typst.enable = true;
      };
    };
    themes.active = "kanagawa";

    desktop = {
      xmonad.enable = true;
      terminal = {
        default = "wezterm";
        wezterm.enable = true;
      };
      editors = {
        default = "nvim";
        neovim.enable = true;
        emacs.enable = true;
      };
      browsers = {
        default = "firefox";
        chromium.enable = true;
        firefox.enable = true;
      };
      education = {
        memory.enable = true;
        vidcom.enable = false;
      };
      toolset = {
        downloader.transmission.enable = true;
        graphics = {
          raster.enable = true;
          vector.enable = true;
        };
        player = {
          music.enable = true;
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

    containers.transmission = {
      enable = false; # TODO: Once fixed -> enable = true;
      username = "alonzo";
      #  password = builtins.readFile config.age.secrets.torBylon.path;
    };
    virtualize.enable = true;
  };

  # KDE-Connect + Start-up indicator
  programs.kdeconnect.enable = true;
  systemd.user.services.kdeconnect-indicator = {
    serviceConfig.ExecStart = "${pkgs.plasma5Packages.kdeconnect-kde}/bin/kdeconnect-indicator";
    wantedBy = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
  };
}
