{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.shell;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.shell = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [shellcheck];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
