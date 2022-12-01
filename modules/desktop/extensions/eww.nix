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
    hm.programs.eww = {
      enable = true;
      configDir = "${config.snowflake.configDir}/eww";
      package =
        let envProto = config.modules.desktop.envProto;
        in (with pkgs; mkMerge [
          (mkIf (envProto == "x11") [ eww ])
          (mkIf (envProto == "wayland") [ eww-wayland ])
        ]);
    };
  };
}
