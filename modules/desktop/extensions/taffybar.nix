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
    nixpkgs.overlays = [ inputs.taffybar.overlay ];

    # 2-Step workaround (https://github.com/taffybar/taffybar/issues/403)
    gtk.iconCache.enable = true;

    services.xserver = {
      gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
      displayManager.sessionCommands = ''
        # 1st-Step Taffybar workaround
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
      '';
    };
    hm.xsession.importedVariables = [ "GDK_PIXBUF_MODULE_FILE" ];

    hm.services.taffybar = {
      enable = true;
      package = pkgs.haskellPackages.raybar;
    };
  };
}
