{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  shotclip = pkgs.writeScriptBin "shotclip" ''
    #!${pkgs.stdenv.shell}
    img="shot_$(date '+%Y-%m-%d_%H-%M-%S').png"
    [ ! -d "${home}/Pictures/Screenshots" ] && mkdir "${home}/Pictures/Screenshots"
    file = "${home}/Pictures/Screenshots/$img"

    sel_fail() {
      dunstify -u critical -i gnome-screenshot "Shotgun" "Failed to take screenshot"
      exit 1
    }

    case $1 in
      -s | selection)
        grab=$(slop -f "%g") || sel_fail
        shotgun -g "$grab" "$file"
      ;;

      -w | window)
        shotgun -i "$(xdotool getactivewindow)" "$file"
      ;;

      -f | fullscreen)
        shotgun "$file"
      ;;

      *)
        printf '%b' "\
          Take screenshots with shotgun
          <===========================>
          Usage:
          -f, full         Take fullscreen screenshot
          -w, window       Take screenshot of focused window
          -s, selection    Take screenshot of interactively chosen window or rectangle
          -d, shadow       Add background shadow to screenshots
          -h, help         Display all available options\n"
        exit 0
      ;;
    esac

    case $2 in
      -d | shadow)
        convert "$file" \( +clone -background black -shadow 75x10+0+0 \) \
          +swap -bordercolor none -border 10 -background none -layers merge +repage "$file"
      ;;
    esac

    xclip -selection clipboard -target image/png -i "$file"

    ACTION=$(dunstify -u low -A "default,Open" -I "$file" "Shotgun" "Screenshot saved!")

    case "$ACTION" in
      "default") mimeopen "$file" ;;
      "2") rm "$file" ;;
    esac
  '';

in { home.packages = [ shotclip ]; }
