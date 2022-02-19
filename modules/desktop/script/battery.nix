{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop;
in {
  config = mkIf (cfg.xmonad.enable || cfg.bspwm.enable) {
    user.packages = with pkgs;
      [
        (writeScriptBin "batStat" ''
          while :; do
            bat="$(cat /sys/class/power_supply/BAT0/capacity)"
            bat_state="$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep state | tr -d "[:space:]" | cut -c 7-)"

            if [ "$bat" -lt 20 ]; then
                bat_icon=""
            elif [ "$bat" -ge "20" ] && [ "$bat" -lt "40" ]; then
                bat_icon=""
            elif [ "$bat" -ge "40" ] && [ "$bat" -lt "60" ]; then
                bat_icon=""
            elif [ "$bat" -ge "60" ] && [ "$bat" -lt "90" ]; then
                bat_icon=""
            elif [ "$bat" -eq "100" ]; then
                bat_icon=""
            fi

            case "$bat_state" in
              charging)
                bat="$bat%"
                bat_icon=""
                ;;

              *)
                bat="''${bat}%"
                ;;
            esac

            echo "''${bat_icon} $bat"
            sleep 120
          done
        '')
      ];
  };
}
