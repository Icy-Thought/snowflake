{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.hardware.printer = { enable = mkBoolOpt false; };

  config = mkIf config.modules.hardware.printer.enable {
    services.printing = {
      enable = true;
      drivers = [ pkgs.epson-escpr pkgs.samsung-unified-linux-driver ];
    };

    # Enable wireless access to printers
    services.avahi = {
      enable = true;
      nssmdns = true;
      openFirewall = true;
    };
  };
}
