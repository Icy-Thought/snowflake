{ config, options, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.services.rustdesk = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption false; };

  config = mkIf config.modules.services.rustdesk.enable {
    user.packages = [ pkgs.rustdesk ];
  };
}
