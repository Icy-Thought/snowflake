{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.osint;
in {
  options.modules.desktop.appliances.osint = {
    enable = mkBoolOpt false;
    minimal.enable = mkBoolOpt false;
    network.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # Sherlock missing..
      (mkIf cfg.minimal.enable theharvester sn0int)
      (mkIf cfg.network.enable nmap wireshark)
    ];
  };
}
