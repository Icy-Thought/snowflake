{ options, config, lib, pkgs, ... }:

let inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.hardware.pointer = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "pointer control"; };

  config = mkIf config.modules.hardware.pointer.enable (mkMerge [
    {
      services.libinput = {
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

    (mkIf (config.modules.desktop.type == "x11") {
      services.unclutter-xfixes = {
        enable = true;
        extraOptions = [ "exclude-root" "ignore-scrolling" "fork" ];
        threshold = 1;
        timeout = 1;
      };
    })
  ]);
}
