{ options, config, lib, pkgs, ... }:

let
  inherit (lib) attrValues getExe mkIf mkOption makeBinPath;

  cfg = config.modules.desktop.extensions.elkowar;
  envProto = config.modules.desktop.envProto;
in {
  options.modules.desktop.extensions.elkowar = let
    inherit (lib.types) package;
    inherit (lib.my) mkBoolOpt;
  in {
    enable = mkBoolOpt false;
    package = mkOption {
      type = package;
      default = if (envProto == "wayland") then pkgs.eww-wayland else pkgs.eww;
    };
  };

  config = mkIf cfg.enable {
    # Allow tray-icons to be displayed:
    hm.services.status-notifier-watcher.enable = true;

    # WARN: Error retrieving accessibility bus address: org.freedesktop.DBus.Error.ServiceUnknown: The name org.a11y.Bus was not provided by any .service files
    services.gnome.at-spi2-core.enable = true;

    hm.programs.eww = {
      enable = true;
      configDir = "${config.snowflake.configDir}/elkowar";
      package = cfg.package;
    };

    hm.systemd.user.services.eww = {
      Unit = {
        Description = "Elkowars wacky widgets => daemon.";
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Environment = let
          dependencies = attrValues ({
            inherit (pkgs) bash coreutils mpc_cli util-linux wmctrl;
          });
        in "PATH=/run/wrappers/bin:${makeBinPath dependencies}";
        ExecStart = "${getExe cfg.package} daemon --no-daemonize";
        Restart = "on-failure";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
