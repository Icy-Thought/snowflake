{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [
    ../home.nix
    ./hardware-configuration.nix
  ];

  ## Modules
  modules.themes = {active = "catppuccin";};

  modules.desktop = {
    xmonad.enable = true;
    terminal = {
      default = "alacritty";
      alacritty.enable = true;
    };
    editors = {
      default = "nvim";
      nvim.enable = true;
    };
    browsers = {
      default = "brave";
      brave.enable = true;
    };
    media = {
      mpv.enable = true;
      spotify.enable = true;
      docViewer.enable = true;
    };
  };

  modules.develop = {
    python.enable = true;
    rust.enable = true;
  };

  modules.shell = {
    git.enable = true;
    fish.enable = true;
    gnupg.enable = true;
  };

  ## Local config
  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;

  networking = {
    networkmanager.enable = true;
    useDHCP = false;
  };
}
