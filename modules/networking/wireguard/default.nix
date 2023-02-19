{ options, config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.networking.wireguard = { enable = mkBoolOpt false; };

  config = mkIf config.modules.networking.wireguard.enable {
    user.packages = [ pkgs.wireguard-tools ];

    networking = {
      iproute2.enable = true;

      firewall = {
        allowedTCPPorts = [ 53 ];
        allowedUDPPorts = [ 53 51820 ];
      };
    };
  };
}
