{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  weather = pkgs.writeScriptBin "weather" ''
    #!${pkgs.stdenv.shell}
    refresh() {
      for _ in 1 2 3 4 5; do
        if ping -q -c 1 -W 1 8.8.8.8 >/dev/null 2>&1; then
          weather=$(curl -s wttr.in/?format="%x+%t\n") && break
        else
          sleep 2s
        fi
      done

      [ -z "$weather" ] && return

      condition="$"{weather% *}
      temperature="$"{weather##* }

      hour="$"(date +%H)
      night_yet() {
        [ "$hour" -ge 19 ] && icon=$*
        [ "$hour" -le 4 ] && icon=$*
      }

      case $condition in
        "?") icon="" ;;
        "mm") icon="" ;;

        "=")
          icon=""
          night_yet ""
        ;;

        "///") icon="" ;;
        "//") icon="" ;;
        "**") icon="" ;;
        "*/*") icon="" ;;

        "/")
          icon=""
          night_yet ""
        ;;

        ".")
          icon=""
          night_yet ""
        ;;

        "x")
          icon=""
          night_yet ""
        ;;

        "x/")
          icon=""
          night_yet ""
          ;;

        "*")
          icon=""
          night_yet ""
        ;;

        "*/")
          icon=""
          night_yet ""
        ;;

        "m")
          icon=""
          night_yet ""
        ;;

        "o")
          icon=""
          night_yet ""
        ;;

        "/!/") icon="" ;;
        "!/") icon="" ;;

        "*!*")
          icon=""
          night_yet ""
        ;;

        "mmm") icon="" ;;
        *) icon=$condition ;;
      esac
    }

    myBar() {
      set -- $(xrdb -q | grep -E '*.color0:|*.color3:|*.color7:' | cut -f2 | tr '\n' ' ')

      background=$1
      yellow=$2
      white=$3

      while :; do
        refresh

        if [ -z "$weather" ]; then
          printf 'Offline\n'
          sleep 5m

        else
          printf '<fc=%s,%s:5>%s</fc><fc=%s,%s:5> %s</fc>\n' \
            "$yellow" "$background" "$icon" "$white" "$background" "$temperature"
          sleep 15m

        fi
      done
    }

    case $1 in
      bar) myBar ;;

      *)
        refresh
        [ -z "$weather" ] &&
                printf "\033[01;31mERROR:\033[0m Check your network connection.\n" ||
                printf "%s  %s\n" "$icon" "$temperature"
      ;;
    esac
  '';

in { home.packages = [ weather ]; }
