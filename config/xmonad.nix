{ config, lib, pkgs, ... }:

let
  xmonad = ''
    userresources = "${config.xdg.configHome}"/x11/Xresources
    [ -f "$userresources" ] && xrdb -merge "$userresources"
  '';

in {
  imports = [ ./xresources ./gtk/xmonad.nix ./dunst/xmonad.nix ./rofi ];

  services = {
    gnome-keyring.enable = true;
    status-notifier-watcher.enable = true;
    network-manager-applet.enable = true;
    blueman-applet.enable = true;

    xidlehook = {
      enable = true;
      not-when-audio = true;
      not-when-fullscreen = true;
      environment = {
        "primary-display" = "$(xrandr | awk '/ primary/{print $1}')";
      };
      timers = [
        {
          delay = 60;
          command = ''xrandr --output "$PRIMARY_DISPLAY" --brightness .1'';
          canceller = ''xrandr --output "$PRIMARY_DISPLAY" --brightness 1'';
        }
        {
          delay = 180;
          command = "betterlockscreen -l dim";
        }
        {
          delay = 300;
          command = "systemctl suspend";
        }
      ];
    };

    random-background = {
      enable = true;
      display = "fill";
      imageDirectory = "%h/Pictures/Wallpapers/Randomize";
    };

    taffybar = {
      enable = true;
      package = pkgs.haskellPackages.icy-taffybar;
    };

    kdeconnect = {
      enable = true;
      indicator = true;
    };

  };

  xsession = {
    enable = true;
    numlock.enable = true;

    initExtra = xmonad;
    windowManager.command = ''
      ${pkgs.haskellPackages.icy-xmonad}/bin/icy-xmonad
    '';

    importedVariables = [ "GDK_PIXBUF_MODULE_FILE" ];
  };

  xdg.configFile = {
    "betterlockscreenrc".text = ''
      font="JetBrainsMono Nerd Font"
    '';
  };

}
