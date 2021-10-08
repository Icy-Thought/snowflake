{ inputs, config, lib, pkgs, ... }: {

  users = { # Don't forget to set password through `passwd`!
    defaultUserShell = pkgs.fish;
    mutableUsers = false;

    users = {
      "${config.user.name}" = {
        isNormalUser = true;
        createHome = true;
        useDefaultShell = true;
        extraGroups = [ "adbusers" ];
        hashedPassword =
          "$6$DMQjZ0Nn8JAb$2MBYjRZvhACwUJrDXI6GciNglr.KM3Yaza4CMUaG8HCxOJ2EtRqZZKvTBzRhIPQWjKiYeU3cCpntQNkToiUeu0";
      };
    };
  };

}
