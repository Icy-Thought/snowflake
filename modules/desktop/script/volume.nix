{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop;
in {
  config = mkIf (cfg.xmonad.enable || cfg.bspwm.enable) {
    user.packages = with pkgs;
      [
        (writeScriptBin "set-volume" ''
          pamixer="${pkgs.pamixer}/bin/pamixer"
          dunstify="${pkgs.dunst}/bin/dunstify"

          function printHelp() {
            echo "Usage: $0 [command]
                  - Increase Volume: [up]
                  - Decrease Volume: [down]
                  - Mute:            [toggle]
                  - Volume Status:   [status]"
          }

          function get_volume {
            $pamixer --get-volume
          }

          function is_muted {
            $pamixer --get-mute
          }

          function volume_icon {
            vol=$(get_volume)

            if [[ "$vol" -ge "0" && "$vol" -lt "30" ]]; then
              icon=""
            elif [[ "$vol" -ge "30" && "$vol" -lt "60" ]]; then
              icon="奔"
            elif [[ "$vol" -ge "60" && "$vol" -lt "90" ]]; then
              icon="墳"
            elif [[ "$vol" -ge "90" && "$vol" -le "100" ]]; then
              icon="墳"
            else
              icon="墳"
            fi

            echo "$icon"
          }

          function notifySend {
            if [[ $(is_muted) == true ]]; then
              dunstify "$(volume_icon) 0%" \
                -a Volume \
                -t 1000 \
                -h string:x-dunst-stack-tag:volume \
                -u low "Muted"
            else
              volume=$(get_volume)
              bar=$(seq -s "─" $(($volume / 4)) | sed 's/[0-9]//g')

              dunstify "$(volume_icon) ''${volume}%" \
                -a Volume \
                -t 1000 \
                -h string:x-dunst-stack-tag:volume \
                -u low "$bar"
            fi
          }

          function volume() {
            if [[ $(is_muted) == true ]]; then
              $pamixer --toggle-mute
            elif [[ $1 == "up" ]]; then
              $pamixer --increase 5
            elif [[ $1 == "down" ]]; then
              $pamixer --decrease 5
            fi
          }

          function volume_status() {
            while :; do
              volume=$(get_volume)

              echo "$(volume_icon) ''${volume}%"
              sleep 60
            done
          }

          case "$1" in
            -h|--help|-\?)
              printHelp
              exit
              ;;

            up)
              volume $1
              notifySend
              ;;

            down)
              volume $1
              notifySend
              ;;

            toggle)
              $pamixer --toggle-mute
              notifySend
              ;;

            status)
              trap 'volume_status' USR1
              volume_status
              ;;

            *)
              printHelp
              exit 1
              ;;
          esac
        '')
      ];
  };
}
