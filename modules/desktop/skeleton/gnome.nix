{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.gnome = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.gnome.enable {
    modules.desktop = {
      envProto = "wayland";
      extensions.ibus.enable = true;
    };

    programs.dconf.enable = true;

    services.xserver = {
      enable = true;
      desktopManager.gnome = {
        enable = true;
        # debug = true;
      };
    };

    services.gnome = {
      gnome-keyring.enable = true;
      chrome-gnome-shell.enable = true;
      sushi.enable = true;
    };

    services.dbus = {
      enable = true;
      packages = with pkgs; [ dconf ];
    };

    services.udev = {
      packages = with pkgs; [ gnome.gnome-settings-daemon ];
      extraRules = ''
        ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
        ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
        ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
      '';
    };

    user.packages = with pkgs;
      [ dconf2nix ] ++ (with gnome; [
        polari
        gnome-disk-utility
        gnome-tweaks
      ]) ++ (with gnomeExtensions; [
        blur-my-shell
        gsconnect
        user-themes
      ]);

    # Force-enable wayland on FireFox
    environment.variables = {
      MOZ_ENABLE_WAYLAND = "1"; # (Firefox) wayland awareness!
    };

    # Enable chrome-gnome-shell in FireFox nightly (mozilla-overlay):
    home.file.chrome-gnome-shell = {
      target = ".mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";
      source = "${pkgs.chrome-gnome-shell}/lib/mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";
    };
  };
}
