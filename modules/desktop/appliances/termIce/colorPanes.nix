{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.appliances.termIce.colorPanes;
  screenDir = "${config.user.home}/Pictures/Screenshots";
in {
  options.modules.desktop.appliances.termIce.colorPanes = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ (writeScriptBin "colorPanes" "") ];
  };
}
