{ options, config, lib, pkgs, ...

}:

let
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.extensions.eww;
in {
  options.modules.desktop.extensions.eww = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # Allow tray-icons to be displayed:
    hm.services.status-notifier-watcher.enable = true;

    hm.programs.eww = {
      enable = true;
      configDir = "${config.snowflake.configDir}/eww"; # TODO
      package = let
        inherit (pkgs) eww eww-wayland;
        envProto = config.modules.desktop.envProto;
      in mkMerge [
        (mkIf (envProto == "x11") [ eww ])
        (mkIf (envProto == "wayland") [ eww-wayland ])
      ];
    };
  };
}
