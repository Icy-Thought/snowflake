{ pkgs
, config
, lib
, ...
}: {
  imports = [
    ../home.nix
    ./hardware-configuration.nix
  ];

  ## Modules
  modules.themes = { active = "catppuccin"; };

  modules.desktop = {
    xmonad.enable = true;
    terminal = {
      default = "kitty";
      kitty.enable = true;
    };
    editors = {
      default = "nvim";
      neovim.enable = true;
    };
    browsers = {
      default = "firefox";
      brave.enable = true;
    };
    media.player = {
      music.enable = true;
      video.enable = true;
    };
    document.sioyek.enable = true;
  };

  modules.develop = {
    python.enable = true;
    rust.enable = true;
  };

  modules.shell = {
    git.enable = true;
    zsh.enable = true;
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
