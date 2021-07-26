{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
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

in { home.packages = [ tray-padding-icon ]; }
