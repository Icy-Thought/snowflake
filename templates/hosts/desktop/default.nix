{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [../home.nix ./hardware-configuration.nix];

  ## Modules
  modules.themes = {active = "catppuccin";};

  modules.desktop = {
    xmonad.enable = true;
    terminal = {
      default = "wezterm";
      wezterm.enable = true;
    };
    editors = {
      default = "nvim";
      neovim.enable = true;
    };
    browsers = {
      default = "firefox";
      firefox.enable = true;
    };
    extensions.player = {
      music.enable = true;
      video.enable = true;
    };
    toolset.docView = {sioyek.enable = true;};
  };

  modules.develop = {
    python.enable = true;
    rust.enable = true;
  };

  modules.shell = {
    zsh.enable = true;
    toolset = {
      git.enable = true;
      gnupg.enable = true;
    };
  };

  ## Local config
  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;

  networking = {
    networkmanager.enable = true;
    useDHCP = false;
  };
}
