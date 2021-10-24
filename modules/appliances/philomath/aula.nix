{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.appliances.philomath.aula;
in {
  options.modules.appliances.philomath.aula = {
    enable = mkBoolOpt false;
    anki.enable = mkBoolOpt true;
    libre.enable = mkBoolOpt false;
    zoom.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      (if cfg.anki.enable then [ anki ] else [ ])

      ++ (if cfg.libre.enable then [ libreoffice ] else [ ])

      ++ (if cfg.zoom.enable then [ zoom-us ] else [ ]);
  };
}
