{ inputs, options, config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.extensions.picom = {
    enable = mkEnableOption "lightweight X11 compositor";
    animation.enable = mkEnableOption "animated picom";
  };

  config = mkIf config.modules.desktop.extensions.picom.enable {
    systemd.user.services.picom = {
      description = "Picom composite manager";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];

      serviceConfig = {
        ExecStart = "${
            getExe pkgs.unstable.picom
          } --config ${config.snowflake.configDir}/picom.conf";
        RestartSec = 3;
        Restart = "always";
      };
    };
  };
}
