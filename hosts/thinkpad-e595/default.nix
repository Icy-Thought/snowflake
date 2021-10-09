{ pkgs, config, lib, ... }: {

  imports = [ ./hwCfg.nix ];

  # Hardware-related Modules:
  modules.hardware.audio.enable = true;
  modules.hardware.audio.pulseAudio.enable = true;
  modules.hardware.audio.alsa.enable = true;
  modules.hardware.openrazer.enable = true;

  # Networking-related Modules:
  modules.networking.enable = true;
  modules.networking.networkManager = true;
  modules.networking.wireguard.enable = true;

  # XMonad-related Modules:
  modules.desktop.inputMF.fcitx5.enable = true;
  modules.desktop.inputMF.spellCheck.enable = true;
  modules.desktop.envManager.xmonad.enable = true;
  modules.desktop.envDisplay.sddm.enable = true;
  modules.desktop.envExtra.taffybar.enable = true;

  # Extras Modules for XMonad:
  modules.desktop.envExtra.picom.enable = true;
  modules.desktop.envExtra.gtk.enable = true;
  modules.desktop.envExtra.rofi.enable = true;
  modules.desktop.envExtra.dunst.enable = true;

  # Terminal-related Modules:
  modules.desktop.termEmu.default = "kitty";
  modules.desktop.termEmu.kitty.enable = true;
  modules.desktop.termEmu.alacritty.enable = true;
  modules.desktop.txtEditor.emacs.enable = true;

  # WM-Script Modules:
  modules.desktop.envScripts.brightness.enable = true;
  modules.desktop.envScripts.microphone.enable = true;
  modules.desktop.envScripts.screenshot.enable = true;
  modules.desktop.envScripts.volume.enable = true;

  # Browser-related Modules:
  modules.desktop.defBrowsers.default = "firefox";
  modules.desktop.defBrowser.firefox.enable = true;
  modules.desktop.defBrowser.ungoogled.enable = true;

  # Random Application Modules:
  modules.desktop.defExtra.chat.enable = true;
  modules.desktop.defExtra.docViewer.enable = true;
  modules.desktop.defStudy.aula.anki.enable = true;
  # modules.desktop.virtEnv.vbox.enable = true;

  # Media-related Modules:
  modules.desktop.defMedia.mpv.enable = true;
  modules.desktop.defMedia.spotify.enable = true;
  modules.desktop.defMedia.graphics.enable = true;

  # Gaming-related Modules:
  modules.desktop.defGaming.steam.enable = true;
  modules.desktop.defGaming.lutris.enable = true;

  # Font-related Modules:
  modules.fonts.enable = true;

  # Development-related Modules:
  modules.develop.cc.enable = true;
  modules.develop.haskell.enable = true;
  modules.develop.rust.enable = true;
  # modules.develop.python.enable = true;
  # modules.develop.shell.enable = true;

  # Services-related Modules:
  modules.services.xserver.enable = true;
  modules.services.kdeconnect.enable = true;
  modules.services.laptop.enable = true;
  modules.services.transmission.enable = true;

  # Shell-related Modules:
  modules.shell.adb.enable = true;
  modules.shell.git.enable = true;
  modules.shell.bash.enable = true;
  modules.shell.fish.enable = true;
  modules.shell.tmux.enable = true;
  modules.shell.starship.enable = true;
  modules.shell.direnv.enable = true;
  modules.shell.htop.enable = true;
  modules.shell.neofetch.enable = true;
  modules.shell.printTermColors.enable = true;

  boot.kernel.sysctl."abi.vsyscall32" = 0; # League of Legends..
  boot.kernelParams = [ "acpi_backlight=native" ];

  # Remove device entry from file-manager:
  fileSystems."/".device = "/dev/disk/by-label/nixos";
  fileSystems."/".fsType = "ext4";
  filesystems."/".options = [ "noatime, x-gvfs-hide" ];

  fileSystems."/boot".device = "/dev/disk/by-label/boot";
  fileSystems."/boot".fsType = "vfat";
  fileSystems."/boot".options = [ "x-gvfs-hide" ];

  fileSystems."/home".device = "/dev/disk/by-label/home";
  fileSystems."/home".fsType = "ext4";
  fileSystems."/home".options = [ "noatime, x-gvfs-hide" ];

  hardware.cpu.amd.updateMicrocode = true;
  hardware.opengl.extraPackages =
    [ pkgs.amdvlk pkgs.driversi686Linux.amdvlk pkgs.rocm-opencl-icd ];

  i18n.defaultLocale = "en_US.UTF-8";
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;
  time.timeZone = "Europe/Berlin";

  user.name = "sirius";
  networking.hostName = "thinkpad-e595";

  services.avahi.enable = false;
  services.gvfs.enable = true;
  systemd.services.systemd-udev-settle.enable = false;

  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  services.libinput.touchpad.accelSpeed = "0.5";
  services.libinput.touchpad.accelProfile = "adaptive";
}
