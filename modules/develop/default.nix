{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop;
in {
  options.modules.develop = {
    enable = mkBoolOpt true;
    xdg.enable = mkBoolOpt true;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      # nixLang related
      user.packages = with pkgs; [alejandra];
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
