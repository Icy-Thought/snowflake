{ config, lib, pkgs, ... }:

let
  pulsepipe = pkgs.writeScriptBin "pulsepipe" ''
    #!${pkgs.stdenv.shell}
    SPEAKER=0
    MICROPHONE=0
    STEP=5

    case $1 in
       -i)
         DATA=$(pamixer --get-volume)
         [ "$DATA" -lt '200' ] && pamixer --allow-boost -u -i $STEP
       ;;

       -d)
         pamixer --allow-boost -u -d $STEP
       ;;

        -t)
          pamixer -t
        ;;

        -mi)
          pamixer --default-source -u -i $STEP
        ;;

        -md)
          pamixer --default-source -u -d $STEP
        ;;

        -mt)
          pamixer --default-source -t
        ;;
    esac

    if [ "$SPEAKER" = '1' ]; then
      STATUSPIPE="/tmp/xmobar_volume"
      [ -p "$STATUSPIPE" ] || mkfifo "$STATUSPIPE"
      DATA=$(pamixer --get-volume-human)

      if [ "$DATA" = 'muted' ]; then
        printf "<fc=#ff6c6b><fn=1></fn></fc> %s\n" "$DATA" >$STATUSPIPE
        exit 0
      else
        DATA=$(pamixer --get-volume)
      fi

      if [ "$DATA" -gt '100' ]; then
        printf "<fc=#51aeed><fn=1></fn>!</fc> %s%%\n" "$DATA" >$STATUSPIPE
      elif [ "$DATA" -ge '60' ]; then
        printf "<fc=#98be65><fn=1></fn></fc> %s%%\n" "$DATA" >$STATUSPIPE
      elif [ "$DATA" -ge '20' ]; then
        printf "<fc=#ecbe8b><fn=1></fn></fc> %s%%\n" "$DATA" >$STATUSPIPE
      else
        printf "<fc=#ff6c6b><fn=1></fn></fc> %s%%\n" "$DATA" >$STATUSPIPE
      fi
    fi

    if [ "$MICROPHONE" = '1' ]; then
      STATUSPIPE2="/tmp/xmobar_mic"
      [ -p "$STATUSPIPE2" ] || mkfifo "$STATUSPIPE2"
      DATA2=$(pamixer --default-source --get-volume-human)

      if [ "$DATA" = 'muted' ]; then
        printf "<fc=#ff6c6b><fn=1></fn></fc> %s\n" "$DATA2" >$STATUSPIPE2
        exit 0
      else
        DATA=$(pamixer --default-source --get-volume)
      fi

      if [ "$DATA" -ge '60' ]; then
        printf "<fc=#98be65><fn=1></fn></fc> %s%%\n" "$DATA2" >$STATUSPIPE2
      elif [ "$DATA" -ge '20' ]; then
        printf "<fc=#ecbe8b><fn=1></fn></fc> %s%%\n" "$DATA2" >$STATUSPIPE2
      else
        printf "<fc=#ff6c6b><fn=1></fn></fc> %s%%\n" "$DATA2" >$STATUSPIPE2
      fi
    fi
  '';

in { home.packages = [ pulsepipe ]; }
