{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.xmonad;
  configDir = config.dotfiles.configDir;

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

in {
  options.modules.desktop.xmonad.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.xmonad.overlay inputs.xmonad-contrib.overlay ];

    environment.systemPackages = with pkgs; [
      haskellPackages.icy-xmonad
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

    environment.etc."X11/keymap.xkb".source = customKeyboardLayout;

    gtk.iconCache.enable = true;

    services.blueman.enable = true;
    services.autorandr.enable = true;

    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.defaultSession = "none+xmonad";

    services.xserver.displayManager.sessionCommands = ''
      # 1st-Step Taffybar workaround
      systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH

      # Set XKB layout = us+hyper on XMonad start:
      ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${customKeyboardLayout} $DISPLAY
    '';

    # 2nd-Step workaround for https://github.com/taffybar/taffybar/issues/403
    # Causes GDK_PIXBUF_MODULE_FILE to be set in xsession. (Step 1)
    services.xserver.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];

    services.xserver.windowManager.session = [{
      name = "xmonad";
      start = ''
        /usr/bin/env icy-xmonad &
        waitPID=$!
      '';
    }];

    # Fix xkbOptions (not loading) issue in Xmonad because of Home-Manager FUCK-UP...
    home.keyboard = null;

    # Core Services:
    services.gnome-keyring.enable = true;
    services.status-notifier-watcher.enable = true;
    services.network-manager-applet.enable = true;
    services.blueman-applet.enable = true;

    # Extras:
    services.xidlehook.enable = true;
    services.xidlehook.not-when-audio = true;
    services.xidlehook.not-when-fullscreen = true;
    services.xidlehook.environment = {

      "primary-display" = "$(xrandr | awk '/ primary/{print $1}')";
    };

    services.xidlehook.timers = [
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

    services.random-background.enable = true;
    services.random-background.display = "fill";
    services.random-background.imageDirectory =
      "%h/Pictures/Wallpapers/Randomize";

    services.taffybar.enable = true;
    services.taffybar.package = pkgs.haskellPackages.icy-taffybar;

    services.kdeconnect.enable = true;
    services.kdeconnect.indicator = true;

    services.xsession.enable = true;
    services.xsession.numlock.enable = true;
    services.xsession.preferStatusNotifierItems = true;

    services.xsession.pointerCursor = {
      name = "Bibata_Amber";
      package = pkgs.bibata-cursors;
      defaultCursor = "left_ptr";
      size = 24;
    };

    services.xsession.initExtra = ''
      userresources = "${config.xdg.configHome}"/x11/Xresources
      [ -f "$userresources" ] && xrdb -merge "$userresources"
    '';

    services.xsession.windowManager.command = ''
      ${pkgs.haskellPackages.icy-xmonad}/bin/icy-xmonad
    '';

    services.xsession.importedVariables = [ "GDK_PIXBUF_MODULE_FILE" ];

    xdg.configFile."betterlockscreenrc".text = ''
      font="JetBrainsMono Nerd Font"
    '';
  };
}
