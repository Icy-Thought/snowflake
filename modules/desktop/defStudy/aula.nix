{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.defStudy.aula;
in {
  options.modules.desktop.defStudy.aula = {
    anki.enable = mkBoolOpt true;
    libre.enable = mkBoolOpt false;
    zoom.enable = mkBoolOpt false;
  };

  config = {
    user.packages = with pkgs; [
      (mkIf cfg.anki.enable anki)
      (mkIf cfg.libre.enable libreoffice)
      (mkIf cfg.zoom.enable zoom-us)
    ];
  };
}
