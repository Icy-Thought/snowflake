{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.nix;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.nix = {
    enable = mkBoolOpt true;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [alejandra];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
