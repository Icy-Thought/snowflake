{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.themes;
  configDir = config.snowflake.configDir;
in {
  options.modules.themes = {
    active = mkOption {
      type = types.str;
      example = "ayu-dark";
      description = "System-wide theme to be applied";
    };

    colors = mkOption {
      type = types.attrs;
      description = "Applies active color-palette";
      apply = v: import ("${configDir}/color-palette" + "/${cfg.active}.nix");
    };

    font = mkOption {
      type = types.str;
      example = "VictorMono Nerd Font Mono";
      description = "System-wide font to be applied";
    };

    fontStyle = mkOption {
      type = types.str;
      example = "SemiBold";
      description = "Font-style to use";
    };
  };
}
