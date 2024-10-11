{ config, lib, pkgs, ... }: {

  imports = [ ./hardware.nix ];

  modules = {
    networking = { networkManager.enable = true; };

    themes.active = "catppuccin";

    desktop = {
      gnome.enable = true;
      terminal = {
        default = "alacritty";
        alacritty.enable = true;
      };
      editors = {
        default = "nvim";
        neovim.enable = true;
      };
      browsers = {
        default = "firefox";
        firefox.enable = true;
      };
      toolset = {
        player.video.enable = true;
        readers = {
          enable = true;
          program = "zathura";
        };
      };
    };

    shell = {
      default = "fish";
      toolset.git.enable = true;
    };
  };
}
