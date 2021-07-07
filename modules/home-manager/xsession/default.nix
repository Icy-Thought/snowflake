{ config, lib, pkgs, ... }: {

  xsession.scriptPath = ".xinitrc";
  xsession.initExtra = ''
    userresources = "$XDG_CONFIG_HOME"/x11/Xresources
    [ -f "$userresources" ] && xrdb -merge "$userresources"

    # Firefox-related:
    export MOZ_X11_EGL=1
    export XDG_SESSION_TYPE=x11
  '';

}
