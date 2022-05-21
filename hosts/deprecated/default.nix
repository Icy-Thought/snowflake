{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [./hwCfg.nix];

  modules.hardware = {
    audio.enable = true;
    touchpad.enable = true;
    # razer.enable = true;
  };

  modules.networking = {
    enable = true;
    networkManager.enable = true;

    wireGuard = {
      enable = true;
      akkadianVPN.enable = true;
    };
  };

  modules.themes = {active = "one-dark";};

  modules.desktop = {
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
      default = "brave";
      brave.enable = true;
    };
    media.viewer = {
      document.enable = true;
      video.enable = true;
    };
  };

  modules.services = {
    kdeconnect.enable = true;
  };

  modules.shell = {
    git.enable = true;
    fish.enable = true;
    gnupg.enable = true;
  };

  hardware.opengl.extraPackages = with pkgs; [
    intel-compute-runtime
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
  ];

  services = {
    upower.enable = true;
    printing.enable = true;

    xserver = {
      videoDrivers = ["modesetting"];
      useGlamor = true;
    };
  };
}
