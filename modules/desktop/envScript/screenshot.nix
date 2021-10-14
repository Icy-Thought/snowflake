{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envScript.screenshot;
  screenDir = "${config.user.home}/Pictures/Screenshots";
in {
  options.modules.desktop.envScript.screenshot = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      shotgun
      hacksaw
      xclip

      (writeScriptBin "screenshot" ''
        current="$(date '+%Y-%m-%d %H:%M:%S')"
        icon="${pkgs.whitesur-icon-theme}/share/icons/WhiteSur-dark/apps/scalable/camera.svg"

        notifyScreenshot() {
          case "$1" in
            "-workspace")
              if [ $? -eq 0 ]; then
                dunstify "Screenshot: Active Workspace Captured." \
                  -i ${screenDir}/Workspace\ "$current".png
              else
                dunstify "Screenshot: Failed...\nError Code: $?" \
                  -i $icon
              fi
            ;;

            "-activeWin")
              if [ $? -eq 0 ]; then
                dunstify "Screenshot: Active Window Captured." \
                  -i ${screenDir}/Active-Window\ "$current".png
              else
                dunstify "Screenshot: Failed...\nError Code: $?" \
                  -i $icon
              fi
            ;;

            "-selection")
              if [ $? -eq 0 ]; then
                dunstify "Screenshot: Selection Captured." \
                  -i ${screenDir}/Selection\ "$current".png
              else
                dunstify "Screenshot: Failed...\nError Code: $?" \
                  -i $icon
              fi
            ;;

            "-copyWorkpace")
              if [ $? -eq 0 ]; then
                dunstify "Screenshot: Active Workspace Copied." \
                  -i $icon
              else
                dunstify "Screenshot: Failed...\nError Code: $?" \
                  -i $icon
              fi
            ;;

            "-copyActiveWin")
              if [ $? -eq 0 ]; then
                dunstify "Screenshot: Active Window Copied." \
                  -i $icon
              else
                dunstify "Screenshot: Failed...\nError Code: $?" \
                  -i $icon
              fi
            ;;

            "-copySelection")
              if [ $? -eq 0 ]; then
                dunstify "Screenshot: Current Selection Copied." \
                  -i $icon
              else
                dunstify "Screenshot: Failed...\nError Code: $?" \
                  -i $icon
              fi
            ;;
          esac
        }

        workspace() {
          shotgun ${screenDir}/Workspace\ "$current".png
          notifyScreenshot -workspace
        }
        activeWin() {
          shotgun -i $(xdotool getactivewindow) ${screenDir}/Active-Window\ "$current".png
          notifyScreenshot -activeWin
        }
        selection(){
          selection=$(hacksaw -f "-i %i -g %g")
          shotgun $selection ${screenDir}/Selection\ "$current".png
          notifyScreenshot -selection
        }

        copyWorkspace() {
          shotgun - | xclip -t 'image/png' -selection clipboard
          notifyScreenshot -copyWorkspace
        }
        copyActiveWin() {
          shotgun -i $(xdotool getactivewindow) - | xclip -t 'image/png' -selection clipboard
          notifyScreenshot -copyActiveWin
        }
        copySelection() {
          selection=$(hacksaw -f "-i %i -g %g")
          shotgun $selection - | xclip -t 'image/png' -selection clipboard
          notifyScreenshot -copySelection
        }

        case "$1" in
          "-workspace") workspace ;;
          "-activeWin") activeWin ;;
          "-selection") selection ;;
          "-copyWorkspace") copyWorkspace ;;
          "-copyActiveWin") copyActiveWin ;;
          "-copySelection") copySelection ;;
          *)
            echo "ERROR: Invalid option $1."
            echo "Valid options: (copy)workspace, (copy)activeWin, (copy)selection." >&2;
            exit 1
            ;;
        esac
      '')
    ];
  };
}
