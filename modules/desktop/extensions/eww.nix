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
        let dsktp = config.modules.desktop;
        in (with pkgs; mkMerge [
          (mkIf dsktp.envProto == "x11" [ eww ])
          (mkIf dsktp.envProto == "wayland" [ eww-wayland ])
        ]);
    };
  };
}
