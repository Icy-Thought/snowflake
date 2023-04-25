{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (builtins) readFile;
  inherit (lib) mkIf mkMerge;
  inherit (lib.strings) optionalString;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.extensions.taffybar;
  taffyDir = "${config.snowflake.configDir}/taffybar";
in {
  options.modules.desktop.extensions.taffybar = {
    withOverlay = mkBoolOpt false;
    withoutRebuild = mkBoolOpt false;
  };

  config = mkMerge [
    {
      # WARN: 2-Step workaround (https://github.com/taffybar/taffybar/issues/403)
      gtk.iconCache.enable = true;

      services.xserver = {
        gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
        displayManager.sessionCommands = ''
          # 1st-Step Taffybar workaround
          systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
        '';
      };

      # WARN: Error retrieving accessibility bus address: org.freedesktop.DBus.Error.ServiceUnknown: The name org.a11y.Bus was not provided by any .service files
      services.gnome.at-spi2-core.enable = true;

      hm.services = {
        # Allow tray-icons to be displayed:
        status-notifier-watcher.enable = true;
        taffybar = {
          enable = true;
          package = pkgs.taffybar.override {
            packages = haskellPackages: with haskellPackages; [ hostname ];
          };
        };
      };
    }

    (mkIf cfg.withOverlay {
      nixpkgs.overlays =
        [ (import (config.snowflake.configDir + "/taffybar/overlay.nix")) ];
    })

    (mkIf (cfg.withoutRebuild) {
      # Symlink necessary files for config to load:
      home.configFile = let active = config.modules.themes.active;
      in {
        taffybar-base = {
          target = "taffybar/taffybar.hs";
          source = "${taffyDir}/taffybar.hs";
          onChange = "rm -rf $XDG_CACHE_HOME/taffybar";
        };
        taffybar-palette = mkIf (active != null) {
          target = "taffybar/palette/${active}.css";
          source = "${taffyDir}/palette/${active}.css";
        };
        taffybar-css = {
          target = "taffybar/taffybar.css";
          text = ''
            ${optionalString (active != null) ''
              @import url("./palette/${active}.css");
            ''}
            ${readFile "${taffyDir}/taffybar.css"}
          '';
        };
      };
    })
  ];
}
