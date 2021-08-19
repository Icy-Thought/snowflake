{ config, lib, pkgs, ... }:

let
  micVol = pkgs.writeScriptBin "set-micvol" ''
    pamixer="${pkgs.pamixer}/bin/pamixer"
    notifySend="${pkgs.libnotify}/bin/notify-send"

    showVolume() {
      level=$($pamixer --default-source --get-volume)
      isMuted=$($pamixer --default-source --get-mute)
      if [ "$isMuted" == "true" ]
      then
        icon=notification-microphone-sensitivity-muted
      elif [ $level -ge 50 ]
      then
        icon=notification-microphone-sensitivity-high
      elif [ $level -ge 25 ]
      then
        icon=notification-microphone-sensitivity-medium
      elif [ $level -eq 0 ]
      then
        icon=notification-microphone-sensitivity-muted
      else
        icon=notification-microphone-sensitivity-low
      fi
      $notifySend \
        -R "$XDG_RUNTIME_DIR/mic-volume.msgid" \
        -i $icon \
        -h int:value:$level \
        ""
    }
    raiseVolume() {
      $pamixer --default-source --increase 5
      showVolume
    }
    lowerVolume() {
      $pamixer --default-source --decrease 5
      showVolume
    }
    toggle() {
      $pamixer --default-source --toggle
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

in { home.packages = [ micVol ]; }
