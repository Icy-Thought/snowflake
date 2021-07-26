{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  ssclip = pkgs.writeScriptBin "ssclip" ''
    #!${pkgs.stdenv.shell}
    img="scrot_$(date '+%Y-%m-%d_%H-%M-%S').png"
    [ ! -d "$HOME/Pictures/Screenshots" ] && mkdir "$HOME/Pictures/Screenshots"
    file="$HOME/Pictures/Screenshots/$img"

    sel_fail() {
      notify-send -u low -i gnome-screenshot 'Scrot' 'Failed to take screenshot'
      exit 1
    }

    case $1 in
      -f | full)
        scrot -z "$file"
      ;;

      -w | window)
        scrot -uz "$file"
      ;;

      -s | selection)
        scrot -l mode=edge -szb "$file" || sel_fail
      ;;

      *)
         printf " Take screenshots with scrot\n"
         printf "<===========================>\n"
         printf " Usage:\n"
         printf "  -f, full         Take fullscreen screenshot\n"
         printf "  -w, window       Take screenshot of focused window\n"
         printf "  -s, selection    Take screenshot of interactively chosen window or rectangle\n"
         printf "  -d, shadow       Add background shadow to screenshots\n"
         printf "  -h, help         Display all available options\n"
         exit 0
      ;;
    esac

    case $2 in
      -d | shadow)
        convert "$file" \( +clone -background black -shadow 75x10+0+0 \) +swap -bordercolor none -border 10 -background none -layers merge +repage "$file"
      ;;
    esac

    xclip -selection clipboard -target image/png -i "$file"
    notify-send -u low -i "$file" 'Scrot' 'Screenshot saved!'
  '';

in { home.packages = [ ssclip ]; }
