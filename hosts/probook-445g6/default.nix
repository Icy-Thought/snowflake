{ pkgs, config, lib, ... }: {
  imports = [ ./hardware.nix ];

  modules = {
    networking = {
      networkManager.enable = true;
      wireGuard.enable = true;
    };

    themes.active = "catppuccin";

    desktop = {
      gnome.enable = true;
      terminal = {
        default = "alacritty";
        alacritty.enable = true;
      };
      editors = {
        default = "nvim";
        neovim.agasaya.enable = true;
      };
      browsers = {
        default = "firefox-devedition";
        firefox.enable = true;
      };
      toolset = {
        player.video.enable = true;
        docView.zathura.enable = true;
      };
    };

    shell = {
      default = "fish";
      git.enable = true;
      gnupg.enable = true;
    };
  };

  services = {
    upower.enable = true;
    printing.enable = true;
    xserver.videoDrivers = [ "modesetting" ];
  };
}
