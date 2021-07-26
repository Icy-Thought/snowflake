{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  wall = pkgs.writeScriptBin "wall" ''
    #!${pkgs.stdenv.shell}
    WALL_DIR = ${home}/Pictures/Wallpapers/Desktop
    CACHE_PATH = ${config.xdg.cacheHome}/wallpaper.sh

    wallpaper() {
      hsetroot -cover "$1"
      cat <<EOF >"$CACHE_PATH"

    hsetroot -cover $1
    EOF

      [ -x "$cached_wall" ] || chmod +x "$cached_wall"
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
      *) $cached_wall || random ;;
    esac
  '';

in { home.packages = [ wall ]; }
