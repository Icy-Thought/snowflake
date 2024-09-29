{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib.modules) mkIf;
  cfg = config.modules.desktop.extensions.picom;
in {
  options.modules.desktop.extensions.picom =
    let inherit (lib.options) mkEnableOption;
    in {
      enable = mkEnableOption "lightweight X11 compositor";
      animation.enable = mkEnableOption "animated picom";
    };

  config = mkIf cfg.enable {
    systemd.user.services.picom = {
      description = "Picom composite manager";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];

      serviceConfig = {
        ExecStart = "${
            lib.getExe pkgs.picom
          } --config ${config.snowflake.configDir}/picom.conf";
        RestartSec = 3;
        Restart = "always";
      };
    };
  };
}
