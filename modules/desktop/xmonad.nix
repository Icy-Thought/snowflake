{ inputs, options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.xmonad;
  configDir = config.snowflake.configDir;
  activeT = config.modules.themes.active;
in {
  options.modules.desktop.xmonad = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.xmonad.overlay inputs.xmonad-contrib.overlay ];

    environment.systemPackages = with pkgs; [
      haskellPackages.icy-xmonad
      dunst
      pavucontrol
      playerctl
      gnome.nautilus
      gxmessage
      xdotool
      feh
    ];

    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+xmonad";
        sddm.enable = true;
        sddm.settings = { General.InputMethod = ""; };
        sessionCommands = ''
          ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
        '';
      };

      windowManager.session = [{
        name = "xmonad";
        start = ''
          /usr/bin/env icy-xmonad &
          waitPID=$!
        '';
      }];
    };

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
      xsession = {
        enable = true;
        numlock.enable = true;
        preferStatusNotifierItems = true;

        windowManager.command = ''
          ${pkgs.haskellPackages.icy-xmonad}/bin/icy-xmonad
        '';

        importedVariables = [ "GDK_PIXBUF_MODULE_FILE" ];
      };
    };
  };
}
