{ config, lib, pkgs, ... }: {

  services.gammastep = {
    enable = true;
    tray = true;
    provider = "manual";
    longitude = "52.5200";
    latitude = "13.4050";
    dawnTime = "22:00";
    duskTime = "06:00";

    temperature = {
      day = "5500";
      night = "3700";
    };
  };

}
