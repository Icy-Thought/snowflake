{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  date = pkgs.writeScriptBin "date" ''
    #!${pkgs.stdenv.shell}
    month=$(date '+%B %Y')
    calendar() {
      today=$(date +%d)
      cal | tail -n7 | sed "s|$today|<u><b>$today</b></u>|"
    }

    dunstify -u low "$month" "$(calendar)"
  '';

in { home.packages = [ date ]; }
