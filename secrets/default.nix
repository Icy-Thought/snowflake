{ config, lib, pkgs, ... }: {

  age.secrets = {
    "Akkadian-VPN/private-key" = {
      file = ./Akkadian-VPN/privateKey.age;
      owner = "${config.user.name}";
      mode = "0440";
    };

    "OpenWeatherMap/privateKey" = {
      file = ./OpenWeatherMap/privateKey.age;
      owner = "${config.user.name}";
      mode = "0440";
    };

  };
}
