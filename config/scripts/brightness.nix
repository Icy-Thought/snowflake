{ config, lib, pkgs, ... }:

let
  brightness = pkgs.writeScriptBin "set-brightness" ''
    brightnessctl="${pkgs.brightnessctl}/bin/brightnessctl"
    dunstify="${pkgs.dunst}/bin/dunstify"

    function printHelp() {
      echo "Usage: $0 [command]
            - Increase Brightness: [up]
            - Decrease Brightness: [down]
            - Mute: [toggle]
            -n|--notify"
    }

    function get_brightness {
      $brightnessctl get
    }

    brightnessIcon() {
      if [[ $1 == "off" ]]; then
        icon=off
      elif (( $1 < 30 )); then
        icon=low
      elif (( $1 < 70 )); then
        icon=medium
      else
        icon=high
      fi
        echo ${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/display-brightness-$icon-symbolic.svg
    }

    function notifySend {
      brightness=$(get_brightness)
      bar=$(seq -s "█" 0 $((brightness / 10 )) | sed 's/[0-9]//g')

      dunstify \
        -a Brightness \
        -i $(brightnessIcon $brightness) \
        -t 1000 \
        -h string:x-dunst-stack-tag:brightness \
        -u low "$bar"

    }

    function brightness() {
      if [[ $1 == "up" ]]; then
        $brightnessctl set 5%+
      elif [[ $1 == "down" ]]; then
        $brightnessctl set 5%-
      fi
    }

    case "$1" in
      -h|--help|-\?)
        printHelp
        exit
        ;;

      up)
        brightness $1
        notifySend
        ;;

      down)
        brightness $1
        notifySend
        ;;

      *)
        printHelp
        exit 1
        ;;

    esac
  '';

in { home.packages = [ brightness ]; }
