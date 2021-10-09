{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envManager.gnome;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.envManager.gnome = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    programs.dconf.enable = true;

    services.xserver.desktopManager.gnome.enable = true;
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.displayManager.gdm.wayland = true;

    services.gnome.gnome-keyring.enable = true;
    services.gnome.chrome-gnome-shell.enable = true;

    services.dbus.enable = true;
    services.dbus.packages = with pkgs; [ gnome.dconf ];

    services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
    services.udev.extraRules = ''
      ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
      ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
      ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
    '';

    user.packages = with pkgs; [
      dconf2nix
      gnome.zenity
      gnome.polari
      gnome.meld
      gnome.gnome-boxes
      gnome.gnome-dictionary
      gnome.gnome-disk-utility
      gnome.gnome-tweak-tool

      # Extras:
      # gnomeExtensions.pop-os-shell
      gnomeExtensions.gsconnect
      gnomeExtensions.user-themes
    ];

    home.sessionVariables = { MOZ_ENABLE_WAYLAND = 1; };

    # Enable chrome-gnome-shell in FireFox nightly (mozilla-overlay):
    home.file.".mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json".source =
      "${pkgs.chrome-gnome-shell}/lib/mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";
  };
}
