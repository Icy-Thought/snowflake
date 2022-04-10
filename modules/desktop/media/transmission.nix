{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.transmission;
in {
  options.modules.desktop.media.transmission = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user = {
      packages = with pkgs; [ transmission-gtk ];
      extraGroups = [ "transmission" ];
    };

    networking.firewall = {
      allowedTCPPorts = [ 51413 ];
      allowedUDPPorts = [ 51413 ];
    };
  };
}
