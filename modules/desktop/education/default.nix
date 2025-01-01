{ options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.education;
in with lib; {
  options.modules.desktop.education = {
    memorization.enable = mkEnableOption "SUID tool (sandbox)";
    vidcom.enable = mkEnableOption "jailed zoom-us";
  };

  config = mkMerge [
    (mkIf cfg.memorization.enable {
      # TODO: Configure anki OR replace with other software
      user.packages = [ pkgs.anki ];
    })

    (mkIf cfg.vidcom.enable {
      programs.firejail = {
        enable = true;
        wrappedBinaries.zoom = {
          executable = "${getExe pkgs.zoom-us}";
          profile = "${pkgs.firejail}/etc/firejail/zoom.profile";
        };
      };

      user.packages = [
        (pkgs.makeDesktopItem {
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
