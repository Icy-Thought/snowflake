{ inputs
, options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.extra.eww;
in
{
  options.modules.desktop.extra.eww = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs.python310Packages; [
      dbus-python
      pygobject3
      mpd2
      requests
      Wand
    ];

    hm.programs.eww = {
      enable = true;
      package = pkgs.eww;
      configDir = "${config.snowflake.configDir}/eww";
    };
  };
}
