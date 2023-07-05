{
  inputs,
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.penrose = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "Rust-based tiling window manager";};

  config = mkIf config.modules.desktop.penrose.enable {
    modules.desktop = {
      envProto = "x11";
      toolset.fileManager = {
        enable = true;
        program = "thunar";
      };
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

    environment.systemPackages = attrValues {
      inherit (pkgs) libnotify playerctl gxmessage xdotool feh;
    };

    services.greetd = {
      settings.initial_session = {command = "none+penrose";};
    };

    services.xserver = {
      windowManager.session = [
        {
          name = "penrose";
          start = ''
            /usr/bin/env orcinusWM &
            waitPID=$!
          '';
        }
      ];
    };

    hm.xsession.windowManager = {
      command = "${getExe pkgs.my.orcinusWM}";
    };
  };
}
