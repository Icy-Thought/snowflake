{ config, lib, pkgs, ... }:

let
  brightness = pkgs.writeScriptBin "set-brightness" ''
    brightnessctl="${pkgs.brightnessctl}/bin/brightnessctl"
    dunstify="${pkgs.dunst}/bin/dunstify"
    input="cat /dev/stdin"

    off="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/display-brightness-off-symbolic.svg"
    low="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/display-brightness-low-symbolic.svg"
    medium="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/display-brightness-medium-symbolic.svg"
    high="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/display-brightness-symbolic.svg"

    notifyBrightness() {
      brightness="$1"
      if [ $brightness -eq 0 ]; then
        dunstify \
          -h string:x-canonical-private-synchronous:brightness "Brightness: " \
          -h int:value:"$brightness" \
          -t 1500 \
          -i $off
      elif [ $brightness -le 30 ]; then
        dunstify \
          -h string:x-canonical-private-synchronous:brightness "Brightness: " \
          -h int:value:"$brightness" \
          -t 1500 \
          -i $low
      elif [ $brightness -le 70 ]; then
        dunstify \
          -h string:x-canonical-private-synchronous:brightness "Brightness: " \
          -h int:value:"$brightness" \
          -t 1500 \
          -i $medium
      else
        dunstify \
          -h string:x-canonical-private-synchronous:brightness "Brightness: " \
          -h int:value:"$brightness" \
          -t 1500 \
          -i $high
      fi
    }

    raiseBrightness() {
      $brightnessctl set 5%+
      notifyBrightness "$input"
    }

    lowerBrightness() {
      $brightnessctl set 5%-
      notifyBrightness "$input"
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
