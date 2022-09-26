{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.hardware.input = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.hardware.input.enable {
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
  };
}
