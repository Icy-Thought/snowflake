{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.networking.wireguard;
  configDir = config.snowflake.configDir;
in {
  options.modules.networking.wireguard = {
    enable = mkBoolOpt false;
    akkadianVPN = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = with pkgs; [ wireguard ];

      systemd.services.systemd-resolved.enable = true;
      systemd.services.systemd-machined.enable = false;

      networking.iproute2.enable = true;
      networking.firewall.allowedTCPPorts = [ 53 ];
      networking.firewall.allowedUDPPorts = [ 53 51820 ];
    }

    (mkIf cfg.akkadianVPN.enable {
      imports = [ "${configDir}/wireguard/akkadianVPN.nix" ];
    })
  ]);
}
