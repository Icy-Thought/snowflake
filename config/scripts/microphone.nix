{ config, lib, pkgs, ... }:

let
  micVol = pkgs.writeScriptBin "set-micVol" ''
    pamixer="${pkgs.pamixer}/bin/pamixer"
    dunstify="${pkgs.dunst}/bin/dunstify"
    input="cat /dev/stdin"

    notifyMuted() {
      volume="$1"
      mute="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/audio-input-microphone-muted-symbolic.svg"

      dunstify \
        -h string:x-canonical-private-synchronous:audio "Muted" \
        -h int:value:"$volume" \
        -t 1500 \
        -i $mute
    }

    notifyAudio() {
      volume="$1"
      $pamixer --default-source --get-mute && notifyMuted "$volume" && return

      high="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/audio-input-microphone-high-symbolic.svg"
      medium="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/audio-input-microphone-medium-symbolic.svg"
      low="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/status/symbolic/audio-input-microphone-low-symbolic.svg"

      if [ $volume -eq 0 ]; then
        notifyMuted "$volume"

      elif [ $volume -le 30 ]; then
        dunstify \
          -h string:x-canonical-private-synchronous:audio "Volume: " \
          -h int:value:"$volume" \
          -t 1500 \
          -i $low
      elif [ $volume -le 70 ]; then
        dunstify \
          -h string:x-canonical-private-synchronous:audio "Volume: " \
          -h int:value:"$volume" \
          -t 1500 \
          -i $medium
      else
        dunstify \
          -h string:x-canonical-private-synchronous:audio "Volume: " \
          -h int:value:"$volume" \
          -t 1500 \
          -i $high
      fi
    }

    raiseVolume() {
      $pamixer --default-source --increase 5
      notifyAudio "$input"
    }

    lowerVolume() {
      $pamixer --default-source --decrease 5
      notifyAudio "$input"
    }

    toggleMute() {
      volume="$pamixer --get-mute"
      $pamixer --default-source --toggle-mute

      if [ "$input" -eq 0 ]
      then
        notifyAudio "$volume"
      else
        notifyMuted "$volume"
      fi
    }

    case "$1" in
      up) raiseVolume ;;
      down) lowerVolume ;;
      toggle) toggleMute ;;
      *)
        echo "ERROR: Invalid option $1.  Valid options: up, down, toggle." >&2;
        exit 1
        ;;
    esac
  '';

in { home.packages = [ micVol ]; }
