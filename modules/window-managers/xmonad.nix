{ config, lib, pkgs, ... }:

let
  imports =
    [ ../nixos/fcitx5.nix ../display-managers/sddm.nix ../../config/picom ];

  customKeyboardLayout = pkgs.writeText "custom-keyboard-layout" ''
    xkb_keymap {
      xkb_keycodes  { include "evdev+aliases(qwerty)" };
      xkb_types     { include "complete"      };
      xkb_compat    { include "complete"      };

      partial modifier_keys
      xkb_symbols "hyper" {
        include "pc+us+inet(evdev)+terminate(ctrl_alt_bksp)"
        key  <RCTL> { [ Hyper_R, Hyper_R ] };
        modifier_map Mod3 { <HYPR>, Hyper_R };
      };

      xkb_geometry  { include "pc(pc104)"     };
    };
  '';

  xmonadPkgs = with pkgs; [ haskellPackages.icy-xmonad ];

  defaultPkgs = with pkgs; [
    betterlockscreen
    pavucontrol
    playerctl
    gnome.nautilus
    gxmessage
    shotgun
    hacksaw
    xdotool
    dconf
    feh
  ];

in {
  inherit imports;

  environment = {
    systemPackages = defaultPkgs ++ xmonadPkgs;
    etc."X11/keymap.xkb".source = customKeyboardLayout;
  };

  gtk.iconCache.enable = true;

  services = {
    blueman.enable = true;

    xserver = {
      displayManager = {
        defaultSession = "none+xmonad";

        sessionCommands = ''
          # Taffybar workaround (Step 2)
          systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH

          # Set XKB layout = us+hyper on XMonad start:
          ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${customKeyboardLayout} $DISPLAY
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
