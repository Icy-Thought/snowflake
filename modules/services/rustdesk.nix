{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.services.rustdesk = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.services.rustdesk.enable {
    user.packages = with pkgs; [ rustdesk ];
  };
}
