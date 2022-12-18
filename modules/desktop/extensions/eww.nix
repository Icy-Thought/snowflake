{ options
, config
, lib
, pkgs
, ...

}:
with lib;
with lib.my;

let cfg = config.modules.desktop.extensions.eww;
in {
  options.modules.desktop.extensions.eww = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hm.services = {
      network-manager-applet.enable = true;
      status-notifier-watcher.enable = true;
    };

    hm.programs.eww = {
      enable = true;
      configDir = "${config.snowflake.configDir}/eww"; # TODO
      package =
        let envProto = config.modules.desktop.envProto;
        in (with pkgs; mkMerge [
          (mkIf (envProto == "x11") [ eww ])
          (mkIf (envProto == "wayland") [ eww-wayland ])
        ]);
    };
  };
}
