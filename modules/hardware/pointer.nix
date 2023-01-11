{ config
, options
, lib
, pkgs
, ...
}:

let inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in
{
  options.modules.hardware.pointer = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.hardware.pointer.enable (mkMerge [
    {
      services.xserver.libinput = {
        enable = true;
        mouse = {
          middleEmulation = false;
          disableWhileTyping = true;
        };
        touchpad = {
          sendEventsMode = "disabled-on-external-mouse";
          scrollMethod = "twofinger";
          naturalScrolling = true;
          tapping = true;
          tappingDragLock = false;
          disableWhileTyping = true;
        };
      };
    }

    (mkIf (config.modules.desktop.envProto == "x11") {
      services.xserver.imwheel = {
        enable = true;
        extraOptions = [ "--buttons=45" ];
        rules = {
          "epiphany|chromium|discord" = ''
            None,      Up,   Button4, 4
            None,      Down, Button5, 4
            Shift_L,   Up,   Shift_L|Button4, 4
            Shift_L,   Down, Shift_L|Button5, 4
            Control_L, Up,   Control_L|Button4
            Control_L, Down, Control_L|Button5
          '';
        };
      };
    })
  ]);
}
