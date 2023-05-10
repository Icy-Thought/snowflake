{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.xmonad = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "haskell (superior) WM"; };

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
        # elkowar.enable = true;
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
      inherit (pkgs) libnotify playerctl gxmessage xdotool clipboard-jh feh;
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
