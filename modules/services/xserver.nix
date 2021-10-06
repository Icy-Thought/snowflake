{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  services.xserver.enable = true;
  services.xserver.layout = "us";

  # Libinput:
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.naturalScrolling = true;
  services.xserver.libinput.touchpad.tapping = true;
  services.xserver.libinput.touchpad.disableWhileTyping = true;
}
