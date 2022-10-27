{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.distraction.lutris;
  wineCfg = config.modules.desktop.virtual.wine;
in
{
  options.modules.desktop.distraction.lutris = {
    enable = mkBoolOpt false;
    league.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf (cfg.enable && wineCfg.enable) {
      user.packages = with pkgs; [ lutris ];
    })

    (mkIf (cfg.enable && !wineCfg.enable) {
      user.packages = with pkgs; [
        lutris
        wineWowPackages.fonts
        wineWowPackages.stagingFull
        winetricks
      ];
    })

    (mkIf cfg.league.enable {
      networking.firewall.allowedTCPPorts = [ 443 ];

      user.packages = with pkgs; [
        (writeScriptBin "lol-launch-script" ''
          reset_environment() {
            echo "Restoring environment to previous state. (abi.vsyscall32=1)"
            sudo -S sysctl -w abi.vsyscall32=1
            exit 0
          }

          main() {
            trap 'reset_environment' INT
            while [[ $exit_status_pass != 0 ]]; do
              sudo sysctl -w abi.vsyscall32=0
              if [ $? -eq 0 ]; then
                exit_status_pass=0
              fi
            done

            [[ $exit_status_pass == 0 ]] && {
              echo "Launching League of Legends..."
              env LUTRIS_SKIP_INIT=1 ${getExe lutris} lutris:rungame/league-of-legends
            } 
            reset_environment
          } 

          main
        '')

        (makeDesktopItem {
          name = "League of Legends";
          desktopName = "League of Legends";
          icon = "league-of-legends";
          exec = "lol-launch-script";
          terminal = false;
          mimeTypes = [ "x-scheme-handler/league-of-legends" ];
          categories = [ "Network" "FileTransfer" "Game" ];
        })
      ];

      home.dataFile.wine-ge =
        let
          version = "GE-Proton7-32";
          name = "wine-lutris-${version}-x86_64";
        in
        {
          target = "lutris/runners/wine/${name}";
          source = builtins.fetchTarball {
            url = "https://github.com/GloriousEggroll/wine-ge-custom/releases/download/${version}/${name}.tar.xz";
            sha256 = "0fbn3z0ykilkyp8x29srkqalfx1680b6i4zr5brfsdfr7yv17gb4";
          };
        };
    })
  ];
}
