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
    modules.hardware.kmonad.enable = true;

    nixpkgs.overlays = [ inputs.xmonad-contrib.overlay ];

    environment.systemPackages = with pkgs; [
      libnotify
      playerctl
      gxmessage
      xdotool
      xclip
      feh
    ];

    services.xserver = {
      displayManager.defaultSession = "none+xmonad";
      windowManager.session = [{
        name = "xmonad";
        start = ''
          /usr/bin/env birostrisWM &
          waitPID=$!
        '';
      }];
    };

    hm.xsession.windowManager = {
      command = "${getExe pkgs.haskellPackages.birostrisWM}";
    };
  };
}
