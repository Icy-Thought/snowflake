{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.osint.secTesting;
in {
  options.modules.desktop.appliances.osint.secTesting = {
    enable = mkBoolOpt false;
    metadata = mkBoolOpt false;
    sandbox = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # TODO: append more packages to metadata + sandbox.
      (mkIf cfg.metadata.enable exiftool)
      (mkIf cfg.sandbox.enable firejail)
    ];
  };
}
