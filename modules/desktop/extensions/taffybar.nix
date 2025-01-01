{ inputs, options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.extensions.taffybar;
in with lib; {
  options.modules.desktop.extensions.taffybar = {
    enable = mkEnableOption "haskell status-bar library";
  };

  config = mkIf cfg.enable {
    create.configFile = let
      active = config.modules.themes.active;
      taffyDir = "${config.snowflake.configDir}/taffybar";
    in {
      taffybar-palette = mkIf (active != null) {
        target = "taffybar/palette/${active}.css";
        source = "${taffyDir}/palette/${active}.css";
      };
      taffybar-css = {
        target = "taffybar/taffybar.css";
        text = ''
          ${optionalString (active != null) ''
            @import url("palette/${active}.css");
          ''}
          ${builtins.readFile "${taffyDir}/taffybar.css"}
        '';
      };
    };

    # 2-step workaround (https://github.com/taffybar/taffybar/issues/403)
    gtk.iconCache.enable = true;

    programs.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
    services.xserver.displayManager.sessionCommands = ''
      # 1st-Step Taffybar workaround
      systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
    '';

    # WARN: Error retrieving accessibility bus address: org.freedesktop.DBus.Error.ServiceUnknown: The name org.a11y.Bus was not provided by any .service files
    services.gnome.at-spi2-core.enable = true;

    hm.services = {
      status-notifier-watcher.enable = true;
      taffybar = {
        enable = true;
        package = pkgs.haskellPackages.raybar;
      };
    };
  };
}
