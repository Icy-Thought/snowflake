{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.gnome;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.gnome = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.dconf.enable = true;

    services.xserver = {
      enable = true;
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
      desktopManager.gnome.enable = true;
    };

    services.gnome = {
      gnome-keyring.enable = true;
      chrome-gnome-shell.enable = true;
    };

    services.dbus = {
      enable = true;
      packages = with pkgs; [gnome.dconf];
    };

    services.udev = {
      packages = with pkgs; [gnome.gnome-settings-daemon];
      extraRules = ''
        ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
        ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
        ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
      '';
    };

    user.packages = with pkgs; [
      dconf2nix
      gnome.polari
      gnome.gnome-disk-utility
      gnome.gnome-tweak-tool

      # gnomeExtensions.pop-os-shell
      gnomeExtensions.gsconnect
      gnomeExtensions.user-themes
    ];

    # Force-enable wayland on FireFox
    environment.variables = {
      MOZ_ENABLE_WAYLAND = 1;
    };

    # Enable chrome-gnome-shell in FireFox nightly (mozilla-overlay):
    home.file.".mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json".source = "${pkgs.chrome-gnome-shell}/lib/mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";
  };
}
