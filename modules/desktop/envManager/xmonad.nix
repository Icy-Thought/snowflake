{ inputs, options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envManager.xmonad;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.envManager.xmonad = {
    enable = mkBoolOpt false;
    customLayout.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      nixpkgs.overlays =
        [ inputs.xmonad.overlay inputs.xmonad-contrib.overlay ];

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
        feh
      ];

      services.xserver.displayManager = {
        defaultSession = "none+xmonad";
        sessionCommands = ''
          # 1st-Step Taffybar workaround
          systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
        '';
      };

      # 2nd-Step workaround for https://github.com/taffybar/taffybar/issues/403
      # Causes GDK_PIXBUF_MODULE_FILE to be set in xsession. (Step 1)
      gtk.iconCache.enable = true;
      services.xserver.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];

      services.xserver.windowManager.session = [{
        name = "xmonad";
        start = ''
          /usr/bin/env icy-xmonad &
          waitPID=$!
        '';
      }];

      # Prevent x11 askPass prompt on git push:
      programs.ssh.askPassword = "";

      services = {
        autorandr.enable = true;
        blueman.enable = true;
      };

      # Fix xkbOptions (not loading) issue in Xmonad because of Home-Manager FUCK-UP...
      homeManager = {
        home.keyboard = null;

        # Core Services:
        services = {
          gnome-keyring.enable = true;
          blueman-applet.enable = true;

          status-notifier-watcher.enable = true;
          network-manager-applet.enable = true;
        };

        # Extras:
        services.xidlehook = {
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
            name = "Bibata_Amber";
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
        font="JetBrainsMono Nerd Font"
      '';
    }

    (mkIf (cfg.customLayout.enable) (let
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
      environment.etc."X11/keymap.xkb".source = customKeyboardLayout;

      homeManager.services.xsession.windowManager.command = ''
        # Set XKB layout = us+hyper on XMonad start:
        ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${customKeyboardLayout} $DISPLAY
      '';
    }))
  ]);
}
