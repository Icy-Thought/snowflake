{ inputs
, options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.xmonad = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.xmonad.enable {
    modules.desktop = {
      envProto = "x11";
      toolset.fileBrowse = {
        nautilus.enable = true;
      };
      extensions = {
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        picom.enable = true;
        dunst.enable = true;
        rofi.enable = true;
        taffybar.enable = true;
      };
    };

    environment.systemPackages = with pkgs; [
      libnotify
      playerctl
      gxmessage
      xdotool
      xclip
      feh
    ];

    services.xserver = {
      enable = true;
      displayManager.defaultSession = "none+xmonad";
      windowManager.session = [{
        name = "xmonad";
        start = ''
          /usr/bin/env birostris &
          waitPID=$!
        '';
      }];
    };

    hm.services = {
      network-manager-applet.enable = true;
      status-notifier-watcher.enable = true;
    };

    hm.xsession = {
      enable = true;
      numlock.enable = true;
      preferStatusNotifierItems = true;
      windowManager.command = "${getExe pkgs.haskellPackages.birostris}";
      importedVariables = [ "GDK_PIXBUF_MODULE_FILE" ]; # Taffybar
    };
  };
}
