{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
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

in { home.packages = [ msclip ]; }
