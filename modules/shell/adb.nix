{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.adb;
in {
  options.modules.shell.adb = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    programs.adb.enable = true;
    users.extraGroups = [ "adbusers" ];
  };
}
