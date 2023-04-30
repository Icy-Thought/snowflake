{ options, config, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.networking.wireguard =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption false; };

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
