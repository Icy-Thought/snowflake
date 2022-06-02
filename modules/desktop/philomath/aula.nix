{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.philomath.aula;
in {
  options.modules.desktop.philomath.aula = {
    anki.enable = mkBoolOpt false;
    zoom.enable = mkBoolOpt false;
  };

  config = mkMerge [
    {
      # Configure anki OR replace with other software
      user.packages = with pkgs; (
        if cfg.anki.enable
        then [anki]
        else []
      );
    }
    (mkIf cfg.zoom.enable {
      programs.firejail = {
        enable = true;
        wrappedBinaries.zoom = {
          executable = "${getExe pkgs.zoom-us}";
          profile = "${pkgs.firejail}/etc/firejail/zoom.profile";
        };
      };

      user.packages = with pkgs; [
        (makeDesktopItem {
          name = "zoom-us";
          desktopName = "Zoom (Jailed)";
          icon = "Zoom";
          exec = "/run/current-system/sw/bin/zoom";
          genericName = "Video Conference";
          categories = ["Network" "VideoConference"];
        })
      ];
    })
  ];
}
