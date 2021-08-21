{ config, ... }: {

  imports = [ ./config/Akkadian-VPN.nix ];

  age.secrets = {
    "Akkadian-VPN/private-key" = {
      file = ../../secrets/wg-akkad/privateKey.age;
      owner = "${config.user.name}";
      mode = "0440";
    };
  };
}
