{ config, lib, pkgs, ... }:

let
  xmonad = ''
    userresources = "${config.xdg.configHome}"/x11/Xresources
    [ -f "$userresources" ] && xrdb -merge "$userresources"
  '';

in {
  imports = [ ./xresources ./gtk ./dunst ./rofi ];

  config.services = {
    gnome-keyring.enable = true;
    status-notifier-watcher.enable = true;

    random-background = {
      enable = true;
      display = "fill";
      imageDirectory = "%h/Pictures/Wallpapers/Randomize";
    };

    taffybar = {
      enable = true;
      package = pkgs.haskellPackages.icy-taffybar;
    };

    # notify-osd = {
    #   enable = true;
    #   package = pkgs.notify-osd-customizable;
    # };

    kdeconnect = {
      enable = true;
      indicator = true;
    };

  };

  config.xsession = {
    enable = true;
    numlock.enable = true;
    initExtra = xmonad;
    windowManager.command = ''
      ${pkgs.haskellPackages.icy-xmonad}/bin/icy-xmonad
    '';

    importedVariables = [ "GDK_PIXBUF_MODULE_FILE" ];
  };

}
