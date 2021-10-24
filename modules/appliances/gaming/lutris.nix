{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.appliances.gaming.lutris;
in {
  options.modules.appliances.gaming.lutris = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      lutris
      openssl
      osu-lazer
      wineWowPackages.fonts
      wineWowPackages.staging
      (winetricks.override { wine = wineWowPackages.staging; })
    ];

    # League of Legends (OpenSSL)
    networking.firewall.allowedTCPPorts = [ 443 ];
  };
}
