{ pkgs, ... }: 
{

  users = {
    defaultUserShell = pkgs.fish;
    mutableUsers = false;

    users.root = {
       initialHashedPassword = "$6$DMQjZ0Nn8JAb$2MBYjRZvhACwUJrDXI6GciNglr.KM3Yaza4CMUaG8HCxOJ2EtRqZZKvTBzRhIPQWjKiYeU3cCpntQNkToiUeu0";
       shell = pkgs.bash;
    };

    users.sirius = {
      isNormalUser = true;
      home = "/home/sirius";
      shell = pkgs.fish;
      extraGroups = [ "wheel" "users" "network" "audio" "video" "storage" "plugdev" "adbusers" ];
      initialHashedPassword = "$6$DMQjZ0Nn8JAb$2MBYjRZvhACwUJrDXI6GciNglr.KM3Yaza4CMUaG8HCxOJ2EtRqZZKvTBzRhIPQWjKiYeU3cCpntQNkToiUeu0";
    };

    users.orca = {
      isNormalUser = true;
      home = "/home/orca";
      shell = pkgs.fish;
      extraGroups = [ "wheel" "users" "network" "audio" "video" "storage" "plugdev" "adbusers" ];
      initialHashedPassword = "$6$Xny1A0ZwSSw/t1$3MUaZ0Cr4nV/N.n2VTWLIg1of8SAzAFm7EA.KRFYXeRRitIfKAAeFLT8AVGxP8NyhYOPkRngclRQjqc5Gmzqb0";
    };
  };

}
