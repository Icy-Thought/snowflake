{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.touchpad;
in {
  options.modules.hardware.touchpad = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver.libinput = {
      enable = true;
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
