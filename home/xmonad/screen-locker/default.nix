{ config, lib, pkgs, ... }: {

  services.screen-locker = {
    enable = true;
    enableDetectSleep = true;
    inactiveInterval = 5;
    lockCmd = "lockctl";
  };

}
