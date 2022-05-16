{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.colorPanes;
  screenDir = "${config.user.home}/Pictures/Screenshots";
in {
  options.modules.shell.colorPanes = {enable = mkBoolOpt false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [(writeScriptBin "colorPanes" "")];
  };
}
