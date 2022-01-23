{ inputs, options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envManager.xmonad;
  configDir = config.snowflake.configDir;
  font = config.modules.themes.font;
  fontStyle = config.modules.themes.fontStyle;
in {
  options.modules.desktop.envManager.xmonad = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.xmonad.overlay inputs.xmonad-contrib.overlay ];

    environment.systemPackages = with pkgs; [
      haskellPackages.icy-xmonad
      betterlockscreen
      pavucontrol
      playerctl
      gnome.nautilus
      gxmessage
      xdotool
      dconf
      feh
    ];

    services.xserver = {
      displayManager.defaultSession = "none+xmonad";

      windowManager.session = [{
        name = "xmonad";
        start = ''
          /usr/bin/env icy-xmonad &
          waitPID=$!
        '';
      }];
    };

    # GTK Theming:
    programs.dconf.enable = true;

    # Prevent x11 askPass prompt on git push:
    programs.ssh.askPassword = "";

    services = {
      autorandr.enable = true;
      blueman.enable = true;
    };

    homeManager = {
      services = {
        gnome-keyring.enable = true;
        blueman-applet.enable = true;

        status-notifier-watcher.enable = true;
        network-manager-applet.enable = true;
      };

      # Extras:
      services.random-background = {
        enable = true;
        display = "fill";
        imageDirectory = "%h/Pictures/Wallpapers/Randomize";
      };

      xsession = {
        enable = true;
        numlock.enable = true;
        preferStatusNotifierItems = true;

        pointerCursor = {
          name = "Bibata-Modern-Amber";
          package = pkgs.bibata-cursors;
          defaultCursor = "left_ptr";
          size = 24;
        };

        windowManager.command = ''
          ${pkgs.haskellPackages.icy-xmonad}/bin/icy-xmonad
        '';

        importedVariables = [ "GDK_PIXBUF_MODULE_FILE" ];
      };
    };

    home.configFile."betterlockscreenrc".text = ''
      font="${font} ${fontStyle}"
    '';
  };
}
