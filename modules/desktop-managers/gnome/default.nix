{ config, lib, pkgs, ... }: {

  programs = { dconf.enable = true; };

  services = {
    gnome = { # Remove after fixing gnome/default.nix
      gnome-keyring.enable = true;
      chrome-gnome-shell.enable = true;
    };

    xserver = {
      desktopManager.gnome = { enable = true; };

      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    };

    dbus = {
      enable = true;
      packages = with pkgs; [ gnome.dconf ];
    };

    udev = {
      extraRules = ''
        ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
        ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
        ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
      '';

      packages = with pkgs; [ gnome.gnome-settings-daemon ];
    };
  };
}
