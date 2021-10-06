{ config, lib, pkgs, ... }:

let imports = [ ../display-managers/gdm.nix ];

in {
  inherit imports;

  programs = { dconf.enable = true; };

  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = true;

  services.gnome.gnome-keyring.enable = true;
  services.gnome.chrome-gnome-shell.enable = true;

  services.dbus.enable = true;
  services.dbus.packages = with pkgs; [ gnome.dconf ];

  services.udev.extraRules = ''
    ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
    ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
    ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
  '';

  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
}
