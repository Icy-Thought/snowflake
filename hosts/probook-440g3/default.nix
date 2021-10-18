{ pkgs, config, lib, ... }: {

  imports = [ ./hwCfg.nix ];

  # Hardware-related Modules:
  modules.hardware = {
    audio.enable = true;
    # openrazer.enable = true;
  };

  # Networking-related Modules:
  modules.networking = {
    enable = true;
    networkManager.enable = true;
    wireGuard.enable = true;
    wireGuard.akkadianVPN.enable = true;
  };

  # XMonad-related Modules:
  modules.desktop = {
    envManager.plasma.enable = true;
    envDisplay.sddm.enable = true;
  };

  modules.fonts.enable = true;
  modules.themes.active = "one-dark";

  modules.desktop.inputMF = { spellCheck.enable = true; };

  # Terminal-related Modules:
  modules.desktop.termEmu = {
    default = "alacritty";
    alacritty.enable = true;
  };

  # Editor-related Modules:
  modules.desktop.txtEditor = {
    default = "emacs";
    emacs.enable = true;
    # nvim.enable = true;
  };

  # Browser-related Modules:
  modules.desktop.defBrowser = {
    default = "firefox";
    firefox.enable = true;
    unGoogled.enable = true;
  };

  # Random Application Modules:
  modules.desktop = {
    defExtra.chat.enable = true;
    defExtra.docViewer.enable = true;
  };

  # Media-related Modules:
  modules.desktop.defMedia = {
    mpv.enable = true;
    spotify.enable = true;
    # graphics.enable = true;
  };

  # Services-related Modules:
  modules.services = {
    xserver.enable = true;
    xserver.touch.enable = true;

    kdeconnect.enable = true;
    laptop.enable = true;
  };

  # Shell-related Modules:
  modules.shell = {
    git.enable = true;
    gnupg.enable = true;
    bash.enable = true;
    fish.enable = true;
    starship.enable = true;
    htop.enable = true;
    neofetch.enable = true;
    printTermColor.enable = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/42c5c3e5-38df-4007-9fff-5c9841c93a0a";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/097C-54E2";
    fsType = "vfat";
    options = [ "x-gvfs-hide" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/e688ecea-6ebd-4740-bd65-6bc27ae2c0db";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  hardware = {
    cpu.intel = { updateMicrocode = true; };

    opengl.extraPackages = with pkgs; [
      intel-compute-runtime
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  networking.hostName = "ProBook-NixOS";

  services.xserver = {
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
  };
}
