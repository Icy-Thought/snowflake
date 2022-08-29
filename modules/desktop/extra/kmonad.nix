{ config
, options
, lib
, pkgs
, ...
}:
with builtins;
with lib;
with lib.my; let
  cfg = config.modules.desktop.extra.kmonad;
  configDir = config.snowflake.configDir;
in
{
  options.modules.desktop.extra.kmonad = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.kmonad = {
      enable = true;
      keyboards.hypedCaps = {
        defcfg = {
          enable = true;
          fallthrough = true;
          allowCommands = false;
          compose = { key = null; };
        };
        # TODO: Fetch from hostname + fix conf
        config = builtins.readFile configDir + "/kmonad/hyped-caps.kbd";
      };
    };
  };
}
