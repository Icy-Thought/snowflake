{ options, config, lib, pkgs, ...

}:

let
  inherit (lib) mkIf;
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
      package = let envProto = config.modules.desktop.envProto;
      in (with pkgs;
        mkMerge [
          (mkIf (envProto == "x11") [ eww ])
          (mkIf (envProto == "wayland") [ eww-wayland ])
        ]);
    };
  };
}
