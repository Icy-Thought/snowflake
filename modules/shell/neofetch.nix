{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell;
  configDir = config.snowflake.configDir;
in {
  options.modules.shell.neofetch = {
    enable = mkBoolOpt false;
  };

  config = mkIf (cfg.neofetch.enable || cfg.fish.enable) {
    user.packages = with pkgs; [neofetch];

    home.configFile."neofetch/config.conf".source = "${configDir}/neofetch/config.conf";
  };
}
