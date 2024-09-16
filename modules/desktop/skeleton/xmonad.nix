{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.xmonad = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Haskell-based (functional) window manager"; };

  config = mkIf config.modules.desktop.xmonad.enable {
    modules.desktop = {
      type = "x11";
      toolset.fileManager = {
        enable = true;
        program = "nautilus";
      };
      extensions = {
        input-method = {
          enable = true;
          framework = "fcitx";
        };
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
      screenshot.enable = true;
    };
    modules.hardware.kmonad.enable = true;

    nixpkgs.overlays = [ inputs.xmonad.overlay inputs.xmonad-contrib.overlay ];

    environment.systemPackages = attrValues {
      inherit (pkgs) libnotify playerctl gxmessage xdotool feh;
      inherit (pkgs.xorg) xwininfo;
    };

    services.xserver.displayManager = {
      session = [{
        manage = "window";
        name = "xmonad";
        start = ''
          systemd-cat -t xmonad -- ${getExe pkgs.haskellPackages.birostrisWM} &
          waitPID=$!
        '';
      }];
      defaultSession = "none+xmonad";
    };
  };
}
