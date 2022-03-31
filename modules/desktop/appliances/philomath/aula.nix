{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.philomath.aula;
in {
  options.modules.desktop.appliances.philomath.aula = {
    enable = mkBoolOpt false;
    anki.enable = mkBoolOpt true;
    zoom.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Configure anki OR replace with other software
      user.packages = with pkgs; (if cfg.anki.enable then [ anki ] else [ ]);
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
