{ config, ... }:

let
  akkadia = ../../secrets/wg-akkad/privateKey.age;

in {
  imports = [
    ./wg-akkad.nix
  ];

  age.secrets = {
    "wg-akkad/privateKey" = {
      file = akkadia;
      owner = "${config.user.name}";
      mode = "0440";
    };
  };
}
