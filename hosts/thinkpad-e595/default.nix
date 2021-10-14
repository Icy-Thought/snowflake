{ pkgs, config, lib, ... }: {

  imports = [ ./hwCfg.nix ];

  # Hardware-related Modules:
  modules.hardware = {
    audio.enable = true;
    openrazer.enable = true;
  };

  # Networking-related Modules:
  modules.networking = {
    enable = true;
    wireGuard.enable = true;
    wireGuard.akkadianVPN.enable = true;
  };

  # XMonad-related Modules:
  modules.desktop = {
    envManager.xmonad.enable = true;
    envDisplay.sddm.enable = true;
    envExtra.taffybar.enable = true;
    envExtra.customLayout.enable = true;
  };

  modules.fonts.enable = true;
  modules.themes.active = "ayu-dark";

  modules.desktop.inputMF = {
    fcitx5.enable = true;
    spellCheck.enable = true;
  };

  # Extras Modules for XMonad:
  modules.desktop.envExtra = {
    picom.enable = true;
    gtk.enable = true;
    rofi.enable = true;
    dunst.enable = true;
  };

  # Terminal-related Modules:
  modules.desktop.termEmu = {
    default = "kitty";
    kitty.enable = true;
    alacritty.enable = true;
  };

  # Editor-related Modules:
  modules.desktop.txtEditor = {
    default = "emacs";
    emacs.enable = true;
    # nvim.enable = true;
  };

  # WM-Script Modules:
  modules.desktop.envScript = {
    brightness.enable = true;
    microphone.enable = true;
    screenshot.enable = true;
    volume.enable = true;
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
    defStudy.aula.anki.enable = true;
    # virtEnv.vbox.enable = true;
  };

  # Media-related Modules:
  modules.desktop.defMedia = {
    mpv.enable = true;
    spotify.enable = true;
    graphics.enable = true;
  };

  # Gaming-related Modules:
  modules.desktop.defGaming = {
    steam.enable = true;
    lutris.enable = true;
  };

  # Development-related Modules:
  modules.develop = {
    cc.enable = true;
    haskell.enable = true;
    rust.enable = true;
    nixLang.enable = true;
    # python.enable = true;
    # shell.enable = true;
  };

  # Services-related Modules:
  modules.services = {
    xserver.enable = true;
    xserver.touch.enable = true;
    # ssh.enable = true;

    kdeconnect.enable = true;
    laptop.enable = true;
    transmission.enable = true;
  };

  # Shell-related Modules:
  modules.shell = {
    adb.enable = true;
    gnupg.enable = true;
    git.enable = true;
    bash.enable = true;
    fish.enable = true;
    tmux.enable = true;
    starship.enable = true;
    direnv.enable = true;
    htop.enable = true;
    neofetch.enable = true;
    printTermColor.enable = true;
  };

  boot.kernel.sysctl."abi.vsyscall32" = 0; # League of Legends..
  boot.kernelParams = [ "acpi_backlight=native" ];

  # Remove device entry from file-manager:
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/a2ee4473-ef03-4cb9-8103-ba4c3d8afb1e";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/3988-91C5";
    fsType = "vfat";
    options = [ "x-gvfs-hide" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/4b1d85cf-c670-4e7e-9b4a-02b3657338dd";
    fsType = "ext4";
    options = [ "noatime, x-gvfs-hide" ];
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    opengl.extraPackages =
      [ pkgs.amdvlk pkgs.driversi686Linux.amdvlk pkgs.rocm-opencl-icd ];
  };

  networking.hostName = "ThinkPad-NixOS";

  systemd.services.systemd-udev-settle.enable = false;

  services = {
    avahi.enable = false;
    gvfs.enable = true;
  };

  services.xserver = {
    videoDrivers = [ "amdgpu" ];
    deviceSection = ''
      Option "TearFree" "true"
    '';
  };

  services.xserver.libinput = {
    touchpad.accelSpeed = "0.5";
    touchpad.accelProfile = "adaptive";
  };
}
