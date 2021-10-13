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
      description = "The active theme.";
    };

    colors = mkOption {
      type = types.attrs;
      description = "Active color set.";
      apply = v: import ("${configDir}/color-palette" + "/${cfg.active}.nix");
    };
  };
}
