{ options, config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkMerge getExe;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.education;
in {
  options.modules.desktop.education = {
    memory.enable = mkBoolOpt false;
    vidcom.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.memory.enable {
      # TODO: Configure anki OR replace with other software
      user.packages = with pkgs; [ anki ];
    })

    (mkIf cfg.vidcom.enable {
      programs.firejail = {
        enable = true;
        wrappedBinaries.zoom = {
          executable = "${getExe pkgs.zoom-us}";
          profile = "${pkgs.firejail}/etc/firejail/zoom.profile";
        };
      };

      user.packages = with pkgs;
        [
          (makeDesktopItem {
            name = "zoom-us";
            desktopName = "Zoom (Jailed)";
            icon = "Zoom";
            exec = "/run/current-system/sw/bin/zoom";
            genericName = "Video Conference";
            categories = [ "Network" "VideoConference" ];
          })
        ];
    })
  ];
}
