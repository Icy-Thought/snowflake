{ config, lib, pkgs, ... }:

let
  brightness = pkgs.writeScriptBin "set-brightness" ''
    brightnessctl="${pkgs.brightnessctl}/bin/brightnessctl"
    notifySend="${pkgs.libnotify}/bin/notify-send"

    showBrightness() {
      level=$((100 * $($brightnessctl get) / $($brightnessctl max)))
      if [ $level -eq 100 ]
      then
        icon=notification-display-brightness-full
      elif [ $level -ge 50 ]
      then
        icon=notification-display-brightness-high
      elif [ $level -ge 25 ]
      then
        icon=notification-display-brightness-medium
      elif [ $level -ge 5 ]
      then
        icon=notification-display-brightness-low
      else
        icon=notification-display-brightness-off
      fi
      $notifySend \
        -R "$XDG_RUNTIME_DIR/brightness.msgid" \
        -i $icon \
        -h int:value:$level \
        ""
    }
    raiseBrightness() {
      $brightnessctl set 5%+
      showBrightness
    }
    lowerBrightness() {
      $brightnessctl set 5%-
      showBrightness
    }
    case "$1" in
      up) raiseBrightness ;;
      down) lowerBrightness ;;
      *)
        echo "ERROR: Invalid option $1.  Valid options: up, down." >&2;
        exit 1
        ;;
    esac
  '';

in { home.packages = [ brightness ]; }
