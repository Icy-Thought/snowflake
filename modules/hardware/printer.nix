{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.hardware.printer = {
    enable = mkEnableOption "printer support";
  };

  config = mkIf config.modules.hardware.printer.enable {
    services.printing = {
      enable = true;
      drivers = [ pkgs.epson-escpr pkgs.samsung-unified-linux-driver ];
    };

    # Enable wireless access to printers
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };
}
