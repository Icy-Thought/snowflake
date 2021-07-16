{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;

  colorclip = pkgs.writeScriptBin "colorclip" ''
    #!${pkgs.stdenv.shell}
    ICON = /tmp/colorclip.png

    xcolor -P 70 | xclip -selection clipboard
    COLOR = $(xclip -selection clipboard -out)

    convert -size 90x90 xc:"$COLOR" "$ICON"
    notify-send -u low -i "$ICON" 'xcolor' "$COLOR"
  '';

  date = pkgs.writeScriptBin "date" ''
    #!${pkgs.stdenv.shell}
    notify-send 'Date' "$(date)" -u low -i gnome-calendar
  '';

  msclip = pkgs.writeScriptBin "msclip" ''
    #!${pkgs.stdenv.shell}
    case $1 in
            -f | full)
                    maim -u | tee ${home}/Pictures/$(date +%d-%m-%Y_%H-%M-%S).png |
                            xclip -selection clipboard -t image/png
                    notify-send --icon=gnome-screenshot 'Maim' 'Fullscreen screenshot saved'
                    ;;
            -w | window)
                    maim -uBi $(xdotool getactivewindow) |
                            tee ${home}/Pictures/$(date +%d-%m-%Y_%H-%M-%S).png |
                            xclip -selection clipboard -t image/png
                    notify-send --icon=gnome-screenshot 'Maim' 'Screenshot of focused window saved'
                    ;;
            -s | selection)
                    maim -us | tee ${home}/Pictures/$(date +%d-%m-%Y_%H-%M-%S).png |
                            xclip -selection clipboard -t image/png
                    notify-send --icon=gnome-screenshot 'Maim' 'Screenshot of selected screen saved'
                    ;;
            *)
                    echo "Take screenshots with maim"
                    echo "---------------------------"
                    echo "options:"
                    echo "-f, full          Take fullscreen screenshot"
                    echo "-w, window        Take screenshot of focused window"
                    echo "-s, selection     Take screenshot of desired region or window"
                    echo "-h, help          Display all available options"
                    ;;
    esac
  '';

  shotclip = pkgs.writeScriptBin "shotclip" ''
    #!${pkgs.stdenv.shell}
    img="shot_$(date '+%m-%d_%H-%M-%S').png"
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

    ACTION=$(dunstify -u low -A "mouse1,Open" -I "$file" "Shotgun" "Screenshot saved!")

    case "$ACTION" in
            "mouse1") mimeopen "$file" ;;
    esac
  '';

  tray-padding-icon = pkgs.writeScriptBin "tray-padding-icon" ''
    #!${pkgs.stdenv.shell}
    create_xpm_icon() {
            timestamp=$(date)
            pixels=$(for i in $(seq "$1"); do printf "."; done)

            cat <<EOF >"$2"
    /* XPM *
    static char * trayer_pad_xpm[] = {
    /* This XPM icon is used for padding in xmobar to */
    /* leave room for stalonetray. It is dynamically  */
    /* updated by by tray-pad-icon.sh which is run by */
    /* xmobar.                                        */
    /* Created: $timestamp */
    /* <w/cols>  <h/rows>  <colors>  <chars per pixel> */
    "$1 1 1 1",
    /* Colors (none: transparent) */
    ". c none",
    /* Pixels */
    "$pixels"
    };
    EOF
    }

    # panel window name
    pname=${"1:-panel"}

    # Width of the trayer window
    width=$(xprop -name "$pname" | grep 'program specified minimum size' | cut -d ' ' -f 5)

    # Icon file name
    iconfile = "/tmp/$pname-padding-${"width:-0"}px.xpm"

    # If the desired icon does not exist create it
    if [ ! -f "$iconfile" ]; then
            create_xpm_icon "$width" "$iconfile"
    fi

    # Output the icon tag for xmobar
    printf "<icon=$iconfile/>"
  '';

  volctl = pkgs.writeScriptBin "volctl" ''
    #!${pkgs.stdenv.shell}
     case $1 in
         -i) amixer -q set 'Master' 5%+ unmute;;
         -d) amixer -q set 'Master' 5%- unmute;;
         -t) amixer -q set 'Master' toggle;;
     esac
  '';

  wall = pkgs.writeScriptBin "wall" ''
    #!${pkgs.stdenv.shell}
    WALL_DIR = ${home}/Pictures/Wallpapers
    CACHE_PATH = ${config.xdg.cacheHome}/wallpaper.sh

    wallpaper() {
            hsetroot -cover "$1"
            cat <<EOF >"$CACHE_PATH"
    #!/bin/sh
    hsetroot -cover $1
    EOF
            [ -x "$CACHE_PATH" ] || chmod +x "$CACHE_PATH"
    }

    random() {
            img=$(find "$WALL_DIR" -type f | grep -v -E 'git|README.md' | shuf -n1)
            hsetroot -cover "$img"
    }

    daemon() {
            while :; do
                    random
                    sleep 10m
            done
    }

    case $1 in
            *.png | *.jpg) wallpaper "$1" ;;
            random) random ;;
            daemon) daemon ;;
            *) "${config.xdg.cacheHome}"/wallpaper.sh || random ;;
    esac
  '';

in {
  home.packages =
    [ colorclip date msclip shotclip tray-padding-icon volctl wall ];
}
