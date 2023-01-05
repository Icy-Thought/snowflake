{ inputs
, options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.extensions.taffybar;
in {
  options.modules.desktop.extensions.taffybar = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # WARN: 2-Step workaround (https://github.com/taffybar/taffybar/issues/403)
    gtk.iconCache.enable = true;

    services.xserver = {
      gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
      displayManager.sessionCommands = ''
        # 1st-Step Taffybar workaround
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
      '';
    };
    hm.xsession.importedVariables = [ "GDK_PIXBUF_MODULE_FILE" ];

    # WARN: Error retrieving accessibility bus address: org.freedesktop.DBus.Error.ServiceUnknown: The name org.a11y.Bus was not provided by any .service files
    services.gnome.at-spi2-core.enable = true;

    hm.services = {
      network-manager-applet.enable = true;
      status-notifier-watcher.enable = true;
      taffybar = {
        enable = true;
        package = pkgs.haskellPackages.raybar;
      };
    };

    # Symlink necessary files for config to load:
    home.configFile =
      let
        taffyDir = "${config.snowflake.configDir}/taffybar";
        active = config.modules.themes.active;
      in
      {
        taffybar-base = {
          target = "taffybar/taffybar.css";
          text = ''
            ${strings.optionalString (active != null) ''
                @import url("./palette/${active}.css");
            ''}
            ${builtins.readFile "${taffyDir}/taffybar.css"}
          '';
        };
        taffybar-theme = mkIf (active != null) {
          target = "taffybar/palette/${active}.css";
          source = "${taffyDir}/palette/${active}.css";
        };
      };
  };
}
