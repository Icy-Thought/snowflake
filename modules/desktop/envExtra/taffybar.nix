{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.envExtra.taffybar;
in {
  options.modules.desktop.envExtra.taffybar = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.taffybar.enable = true;
    services.taffybar.package = pkgs.haskellPackages.icy-taffybar;
  };
}
