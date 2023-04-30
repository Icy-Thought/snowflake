{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.gnome = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption false; };

  config = mkIf config.modules.desktop.gnome.enable {
    modules.desktop = {
      envProto = "wayland";
      extensions.ibus.enable = true;
    };

    programs.dconf.enable = true;

    services.xserver.desktopManager.gnome = {
      enable = true;
      # debug = true;
    };

    services.gnome = {
      chrome-gnome-shell.enable = true;
      sushi.enable = true;
    };

    services.udev = {
      packages = [ pkgs.gnome.gnome-settings-daemon ];
      extraRules = ''
        ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
        ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
        ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
      '';
    };

    user.packages = attrValues ({
      inherit (pkgs) dconf2nix;
      inherit (pkgs.gnome) polari gnome-disk-utility gnome-tweaks;
      inherit (pkgs.gnomeExtensions)
        appindicator aylurs-widgets blur-my-shell dash-to-dock gsconnect
        just-perfection openweather # pop-shell
        removable-drive-menu rounded-window-corners space-bar user-themes;
    });

    # Enable chrome-gnome-shell in FireFox nightly (mozilla-overlay):
    home.file.chrome-gnome-shell = {
      target =
        ".mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";
      source =
        "${pkgs.chrome-gnome-shell}/lib/mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";
    };
  };
}
