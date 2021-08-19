{ config, lib, pkgs, ... }:

let
  volume = pkgs.writeScriptBin "set-volume" ''
    pamixer="${pkgs.pamixer}/bin/pamixer"
    notifySend="${pkgs.libnotify}/bin/notify-send"

    showVolume() {
      level=$($pamixer --get-volume)
      isMuted=$($pamixer --get-mute)
      if [ "$isMuted" == "true" ]
      then
        icon=notification-audio-volume-muted
      elif [ $level -ge 50 ]
      then
        icon=notification-audio-volume-high
      elif [ $level -ge 25 ]
      then
          icon=notification-audio-volume-medium
      elif [ $level -eq 0 ]
      then
          icon=notification-audio-volume-muted
      else
          icon=notification-audio-volume-low
      fi
      $notifySend \
        -R "$XDG_RUNTIME_DIR/volume.msgid" \
        -i $icon \
        -h int:value:$level \
        ""
    }
    raiseVolume() {
      $pamixer --increase 5
      showVolume
    }
    lowerVolume() {
      $pamixer --decrease 5
      showVolume
    }
    toggle() {
      $pamixer --toggle
      showVolume
    }
    case "$1" in
      up) raiseVolume ;;
      down) lowerVolume ;;
      toggle) toggle ;;
      *)
        echo "ERROR: Invalid option $1.  Valid options: up, down, toggle." >&2;
        exit 1
        ;;
    esac
  '';

in { home.packages = [ volume ]; }
