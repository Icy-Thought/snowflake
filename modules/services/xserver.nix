{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.xserver;
in {
  options.modules.services.xserver = {
    enable = mkBoolOpt false;
    touch = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.xserver.enable = true;
      services.xserver.layout = "us";
    }

    (mkIf touchpad.enable {
      services.xserver.libinput.enable = true;
      services.xserver.libinput.touchpad.naturalScrolling = true;
      services.xserver.libinput.touchpad.tapping = true;
      services.xserver.libinput.touchpad.disableWhileTyping = true;
    })
  ]);

}
