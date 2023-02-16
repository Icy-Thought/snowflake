{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.services.rustdesk = { enable = mkBoolOpt false; };

  config = mkIf config.modules.services.rustdesk.enable {
    user.packages = [ pkgs.rustdesk ];
  };
}
