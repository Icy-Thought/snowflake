{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.networking.wireGuard = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.networking.wireGuard.enable {
    user.packages = with pkgs; [ wireguard-tools ];

    networking = {
      iproute2.enable = true;

      firewall = {
        allowedTCPPorts = [ 53 ];
        allowedUDPPorts = [ 53 51820 ];
      };
    };
  };
}
