{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.gaming.selection;
in {
  options.modules.desktop.gaming.selection = {
    geforce.enable = mkBoolOpt true;
    osu.enable = mkBoolOpt false;
  };

  config = {
    user.packages = [
      (mkIf (cfg.geforce.enable) pkgs.my.gfn-electron)
      (mkIf (cfg.osu.enable) pkgs.osu-lazer)
    ];
  };
}
