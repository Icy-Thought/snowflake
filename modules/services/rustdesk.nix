{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.services.rustdesk = {
    enable = mkEnableOption "remote control software";
  };

  config = mkIf config.modules.services.rustdesk.enable {
    user.packages = [ pkgs.rustdesk ];
  };
}
