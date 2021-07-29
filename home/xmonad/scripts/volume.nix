{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  volume = pkgs.writeScriptBin "volume" ''
    #!${pkgs.stdenv.shell}
    set -- $(xrdb -q | grep -E '*.color0:|*.color1:|*.color2:|*.color3:|*.color4:|*.color7:' | cut -f2 | tr '\n' ' ')

    background=$1
    red=$2
    green=$3
    yellow=$4
    blue=$5
    white=$6

    get_volume()  {
      volume=$(pamixer --get-volume-human)
      volume="$"{volume%%%}

      if [ "$volume" = 'muted' ]; then
        printf '<fc=%s,%s:5><fn=1></fn></fc> <fc=%s,%s:5>%s</fc>\n' \
        "$red" "$background" "$white" "$background" "$volume"

      else
        if [ "$volume" -gt '100' ]; then
          printf '<fc=%s,%s:5><fn=1></fn>!</fc> <fc=%s,%s:5>%s%%</fc>\n'  \
          "$blue" "$background" "$white" "$background" "$volume"

        elif [ "$volume" -gt '60' ]; then
          printf '<fc=%s,%s:5><fn=1></fn></fc> <fc=%s,%s:5>%s%%</fc>\n' \
          "$green" "$background" "$white" "$background" "$volume"

        elif [ "$volume" -gt '20' ]; then
          printf '<fc=%s,%s:5><fn=1></fn></fc> <fc=%s,%s:5>%s%%</fc>\n' \
          "$yellow" "$background" "$white" "$background" "$volume"
        else
          printf '<fc=%s,%s:5><fn=1></fn></fc> <fc=%s,%s:5>%s%%</fc>\n' \
          "$red" "$background" "$white" "$background" "$volume"
        fi
      fi
    }

    stdbuf -oL alsactl monitor default | while read -r _; do get_volume; done
  '';

in { home.packages = [ volume ]; }
