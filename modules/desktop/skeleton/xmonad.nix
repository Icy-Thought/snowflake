{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf getExe;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.desktop.xmonad = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.xmonad.enable {
    modules.desktop = {
      envProto = "x11";
      toolset.fileBrowse = { nautilus.enable = true; };
      extensions = {
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        picom = {
          enable = true;
          animation.enable = true;
        };
        dunst.enable = true;
        rofi.enable = true;
        taffybar.enable = true;
      };
    };
    modules.shell.scripts = {
      brightness.enable = true;
      microphone.enable = true;
      volume.enable = true;
      screenshot.enable = true;
    };
    modules.hardware.kmonad.enable = true;

    nixpkgs.overlays = [ inputs.xmonad-contrib.overlay ];

    environment.systemPackages = attrValues ({
      inherit (pkgs) libnotify playerctl gxmessage xdotool xclip feh;
    });

    services.greetd = {
      settings.initial_session = { command = "none+xmonad"; };
    };

    services.xserver = {
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
