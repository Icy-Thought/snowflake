{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.envScript.volume;
in {
  options.modules.desktop.envScript.volume = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        (writeScriptBin "set-volume" ''
          pamixer="${pkgs.pamixer}/bin/pamixer"
          dunstify="${pkgs.dunst}/bin/dunstify"

          function printHelp() {
            echo "Usage: $0 [command]
                  - Increase Volume: [up]
                  - Decrease Volume: [down]
                  - Mute: [toggle]
                  -n|--notify"
          }

          function get_volume {
            $pamixer --get-volume
          }

          function is_muted {
            $pamixer --get-mute
          }

          volumeIcon() {
            if [[ $1 == muted ]]; then
              icon=muted
            elif (( $1 < 30 )); then
              icon=low
            elif (( $1 < 70 )); then
              icon=medium
            else
              icon=high
            fi
              echo ${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/audio-volume-$icon-symbolic.svg
          }

          function notifySend {
            if [[ $(is_muted) == true ]]; then
              dunstify \
                -a Volume \
                -i $(volumeIcon muted) \
                -t 1000 \
                -h string:x-dunst-stack-tag:volume \
                -u low "Muted"
            else
              volume=$(get_volume)
              bar=$(seq -s "█" $(($volume / 5)) | sed 's/[0-9]//g')

              dunstify \
                -a Volume \
                -i $(volumeIcon $volume) \
                -t 1000 \
                -h string:x-dunst-stack-tag:volume \
                -u low "$bar"
            fi
          }

          function volume() {
            if [[ $(is_muted) == true ]]; then
              $pamixer --toggle-mute
            fi

            if [[ $1 == "up" ]]; then
              $pamixer --increase 5
            elif [[ $1 == "down" ]]; then
              $pamixer --decrease 5
            fi

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

            *)
              printHelp
              exit 1
              ;;

          esac
        '')
      ];
  };
}
