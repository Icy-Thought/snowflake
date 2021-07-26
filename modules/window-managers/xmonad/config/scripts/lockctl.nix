{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  lockctl = pkgs.writeScriptBin "lockctl" ''
    #!${pkgs.stdenv.shell}
    set -e

    insidecolor=00000066
    ringcolor=6b8f3d88
    keyhlcolor=abc982ff
    bshlcolor=d22d3a88
    ringvercolor=61afef88
    ringwrongcolor=e06c7588
    timecolor=ffffffff
    datecolor=ffffffff
    loginshadow=00000070

    font='JetBrains Mono'
    locktext='Type password to unlock...'
    time_format='%I:%M %p'
    # LOCK_TIMEOUT=5
    FOLDER="$HOME/.cache/i3lock"

    update_cache() {
      [ ! -d "$FOLDER" ] && mkdir -p "$FOLDER"
      [ -z "$@" ] && exit 1
      TMP="/tmp/lockscreen_cache.png"
      printf "[=                        ](1/6)\r"

      convert "$@" \
        -resize 1920x1080^ -gravity center -extent 1920x1080 \
        "$TMP"
      printf "[=====                    ](2/6)\r"

      convert "$TMP" -blur 0x4 \
        -fill "#$loginshadow" -draw "rectangle 400,1030 40,930" \
        "$FOLDER/lockscreen_blur.png"
      printf "[==========               ](3/6)\r"

      convert "$TMP" -fill black -colorize 40% \
        -fill "#$loginshadow" -draw "rectangle 400,1030 40,930" \
        "$FOLDER/lockscreen_dim.png"
      printf "[===============          ](4/6)\r"

      convert "$TMP" -blur 0x4 -fill black -colorize 40% \
        -fill "#$loginshadow" -draw "rectangle 400,1030 40,930" \
        "$FOLDER/lockscreen_dimblur.png"
      printf "[====================     ](5/6)\r"

      convert "$TMP" \
        -fill "#$loginshadow" -draw "rectangle 400,1030 40,930" \
        "$FOLDER/lockscreen_raw.png"
      printf "[=========================](6/6)\r\n"
    }

    pre_lock() {
      #pacmd suspend 1

      pactl suspend-sink @DEFAULT_SINK@ true
      pactl suspend-source @DEFAULT_SOURCE@ true

      DEF_TIMEOUT=$(eval "(xset q | sed -n '25p')" | cut -d ' ' -f4)
      DEF_DPMS=$(xset q | awk '/^[[:blank:]]*DPMS is/ {print $(NF)}')

      if [ -n "$LOCK_TIMEOUT" ]; then
        xset dpms "$LOCK_TIMEOUT"
      fi

      if [ -n "$(pidof dunst)" ]; then
        pkill -u "$USER" -USR1 dunst
      fi
    }

    post_lock() {
      if [ -n "$LOCK_TIMEOUT" ]; then
        xset dpms "$DEF_TIMEOUT"
        if [ "$DEF_DPMS" = "Disabled" ]; then
          xset -dpms
        fi
      fi

      if [ -n "$(pidof dunst)" ]; then
        pkill -u "$USER" -USR2 dunst
      fi

      #pacmd suspend 0
      pactl suspend-sink @DEFAULT_SINK@ false
      pactl suspend-source @DEFAULT_SOURCE@ false
    }

    i3lock_cmd() {
      if [ -d "${HOME}/.cache/i3lock" ]; then
        style="$"{@:-blur}
        case "$style" in
          blur) IMAGE="$HOME/.cache/i3lock/lockscreen_blur.png" ;;
          dim) IMAGE="$HOME/.cache/i3lock/lockscreen_dim.png" ;;
          dimblur) IMAGE="$HOME/.cache/i3lock/lockscreen_dimblur.png" ;;
          raw) IMAGE="$HOME/.cache/i3lock/lockscreen_raw.png" ;;
          *) $style ;;
        esac
      else
        printf '%b' "\
        \033[1;33mWARNING:\033[0m You haven't chosen a lockscreen wallpaper.
        If you want to set a wallpaper then use the '-u' flag to cache a wallpaper.
        \033[1;34me.g.\033[0m lock -u path/to/image.png\n"
      fi

      [ -n "$IMAGE" ] && WALL='-i'

        i3lock \
          -c 00000000 \
          --blur 3 \
          --force-clock \
          --no-modkey-text \
          --nofork \
          --ignore-empty-password \
          --time-str "$time_format" \
          --date-str "$locktext" \
          --time-color="$timecolor" \
          --date-color="$datecolor" \
          --greeter-color="$datecolor" \
          --time-font="$font" \
          --date-font="$font" \
          --layout-font="$font" \
          --verif-font="$font" \
          --wrong-font="$font" \
          --greeter-font="$font" \
          --verif-size='16' \
          --wrong-size='16' \
          --date-size='14' \
          --layout-size='10' \
          --greeter-size='14' \
          --time-pos='x+140:h-100' \
          --date-pos='x+170:h-75' \
          --ind-pos='x+345:h-100' \
          --inside-color=$insidecolor \
          --ring-color=$ringcolor \
          --keyhl-color=$keyhlcolor \
          --bshl-color=$bshlcolor \
          --ringver-color="$ringvercolor" \
          --ringwrong-color="$ringwrongcolor" \
          --insidever-color="$ringvercolor" \
          --insidewrong-color="$ringwrongcolor" \
          --line-uses-inside \
          --radius=30 \
          --ring-width=3 \
          --verif-text="" \
          --wrong-text="" \
          --noinput-text="" \
          "$WALL" "$IMAGE"
    }

    while :; do
      case "$1" in
        -u | --update)
          update_cache "$2"
          shift 2

          break
        ;;

        -t | --timeout)
          LOCK_TIMEOUT=$2

          shift 2
        ;;

        -l | --lock)
          pre_lock

          i3lock_cmd "$2"

          post_lock

          if [ "$(printf '%s' "$2" | cut -c 1)" = '-' ]; then
            shift 2
          else
            shift 1
          fi

          break
        ;;
      esac
    done
  '';

in { home.packages = [ lockctl ]; }
