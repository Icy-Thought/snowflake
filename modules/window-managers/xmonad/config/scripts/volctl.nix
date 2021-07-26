{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  volctl = pkgs.writeScriptBin "volctl" ''
    #!${pkgs.stdenv.shell}
     case $1 in
         -i) amixer -q set 'Master' 5%+ unmute;;
         -d) amixer -q set 'Master' 5%- unmute;;
         -t) amixer -q set 'Master' toggle;;
     esac
  '';

in { home.packages = [ volctl ]; }
