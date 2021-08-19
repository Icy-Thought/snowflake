{ config, lib, pkgs, ... }:

let
  screenDir = "${config.home.homeDirectory}/Pictures/Screenshots";

  screenshot = pkgs.writeScriptBin "screenshot" ''
    notifySend="${pkgs.libnotify}/bin/notify-send"
    current="$(date '+%Y-%m-%d %H:%M:%S')"
    shotgun="${pkgs.shotgun}/bin/shotgun"
    clipboard="xclip -t 'image/png' -selection clipboard"
    icon=camera

    notify() {
      case "$1" in
        "-workspace")
          if [ $? -eq 0 ]; then
            notify-send "Screenshot: Active Workspace Captured." \
              -i ${screenDir}/Workspace\ "$current".png
          else
            notify-send "Screenshot: Failed...\nError Code: $?" \
              -i $icon
          fi
        ;;

        "-activeWin")
          if [ $? -eq 0 ]; then
            notify-send "Screenshot: Active Window Captured." \
              -i ${screenDir}/Active-Window\ "$current".png
          else
            notify-send "Screenshot: Failed...\nError Code: $?" \
              -i $icon
          fi
        ;;

        "-selection")
          if [ $? -eq 0 ]; then
            notify-send "Screenshot: Selection Captured." \
              -i ${screenDir}/Selection\ "$current".png
          else
            notify-send "Screenshot: Failed...\nError Code: $?" \
              -i $icon
          fi
        ;;

        "-copyWorkpace")
          if [ $? -eq 0 ]; then
            notify-send "Screenshot: Active Workspace Copied." \
              -i $icon
          else
            notify-send "Screenshot: Failed...\nError Code: $?" \
              -i $icon
          fi
        ;;

        "-copyActiveWin")
          if [ $? -eq 0 ]; then
            notify-send "Screenshot: Active Window Copied." \
              -i $icon
          else
            notify-send "Screenshot: Failed...\nError Code: $?" \
              -i $icon
          fi
        ;;

        "-copySelection")
          if [ $? -eq 0 ]; then
            notify-send "Screenshot: Current Selection Copied." \
              -i $icon
          else
            notify-send "Screenshot: Failed...\nError Code: $?" \
              -i $icon
          fi
        ;;
      esac
    }

    workspace() {
      shotgun ${screenDir}/Workspace\ "$current".png
      notify -workspace
    }
    activeWin() {
      shotgun -i $(xdotool getactivewindow) ${screenDir}/Active-Window\ "$current".png
      notify -activeWin
    }
    selection(){
      selection=$(hacksaw -f "-i %i -g %g")
      shotgun $selection ${screenDir}/Selection\ "$current".png
      notify -selection
    }

    copyWorkspace() {
      shotgun - | $clipboard
      notify -copyWorkspace
    }
    copyActiveWin() {
      shotgun -i $(xdotool getactivewindow) - | $clipboard
      notify -copyActiveWin
    }
    copySelection() {
      selection=$(hacksaw -f "-i %i -g %g")
      shotgun $selection - | $clipboard
      notify -copySelection
    }

    case "$1" in
      "workspace") workspace ;;
      "activeWin") activeWin ;;
      "selection") selection ;;
      "copyWorkspace") copyWorkspace;;
      "copyActiveWin") copyActiveWin;;
      "copySelection") copySelection ;;
      *)
        echo "ERROR: Invalid option $1."
        echo "Valid options: (copy)workspace, (copy)activeWin, (copy)selection." >&2;
        exit 1
        ;;
    esac
  '';

  clipScreen = pkgs.writeScriptBin "clip" ''
    }
  '';

in { home.packages = [ screenshot ]; }
