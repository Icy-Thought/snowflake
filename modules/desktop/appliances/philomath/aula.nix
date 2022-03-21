{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.philomath.aula;
in {
  options.modules.desktop.appliances.philomath.aula = {
    enable = mkBoolOpt false;
    anki.enable = mkBoolOpt true;
    libre.enable = mkBoolOpt false;
    zoom.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = with pkgs;
        (if cfg.anki.enable then [ anki ] else [ ])
        ++ (if cfg.libre.enable then [ libreoffice ] else [ ]);
    }
    (mkIf zoom.enable {
      programs.firejail = {
        enable = true;
        wrappedBinaries.zoom = {
          executable = "${lib.getBin pkgs.zoom-us}/bin/zoom-us";
          profile = "${pkgs.firejail}/etc/firejail/zoom.profile";
        };
      };
    })
  ]);
}
