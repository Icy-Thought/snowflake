{ config, lib, pkgs, ... }:

let
  imports =
    [ ../nixos/fcitx5.nix ../display-managers/sddm.nix ../../config/picom ];

  defaultPkgs = with pkgs; [
    autorandr
    xorg.xkbcomp
    betterlockscreen
    pavucontrol
    playerctl
    gnome.nautilus
    gxmessage
    shotgun
    hacksaw
    xdotool
    dconf
    hicolor-icon-theme
  ];

  xmonadPkgs = with pkgs; [ haskellPackages.icy-xmonad dunst feh ];

  myCustomLayout = pkgs.writeText "xkb-layout" ''
    ! Clear the modifiers concerned
    clear mod3

    ! Remove R-Ctrl
    remove control = Control_R

    ! Set the R-Ctrl as Hyper
    keycode 105 = Hyper_R

    ! Add a new Hyper_R modifier mod3
    add mod3 = Hyper_R
  '';

in {
  inherit imports;

  environment.systemPackages = defaultPkgs ++ xmonadPkgs;

  gtk.iconCache.enable = true;

  services = {
    blueman.enable = true;

    xserver = {
      # xkbOptions = "ctrl:swapcaps_hyper,shift:both_capslock";

      displayManager = {
        defaultSession = "none+xmonad";

        sessionCommands = ''
          # Taffybar workaround (Step 2)
          systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH

          ${pkgs.xorg.xmodmap}/bin/xmodmap ${myCustomLayout}
        '';
      };

      # 2-Step workaround for https://github.com/taffybar/taffybar/issues/403
      # Causes GDK_PIXBUF_MODULE_FILE to be set in xsession. (Step 1)
      gdk-pixbuf.modulePackages = [ pkgs.librsvg ];

      windowManager = {
        session = [{
          name = "xmonad";
          start = ''
            /usr/bin/env icy-xmonad &
            waitPID=$!
          '';
        }];
      };
    };
  };

}
