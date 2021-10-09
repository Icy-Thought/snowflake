{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.htop;
  configDir = config.snowflake.configDir;
in {
  options.modules.shell.htop = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ neofetch ];

    home.configFile = {
      "neofetch/config.conf".source = "${configDir}/neofetch/config.conf";
    };
  };
}
