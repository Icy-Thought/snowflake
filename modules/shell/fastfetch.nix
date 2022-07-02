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
  options.modules.shell.fastfetch = {
    enable = mkBoolOpt false;
  };

  # config = mkIf (cfg.fastfetch.enable || cfg.fish.enable) {
  config = mkIf cfg.fastfetch.enable {
    # remove when merged
    user.packages = with pkgs; [fastfetch];

    home.configFile."fastfetch/config.conf".source = "${configDir}/fastfetch/config.conf";
  };
}
