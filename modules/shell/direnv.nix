{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.direnv;
in {
  options.modules.shell.direnv = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    programs.direnv.enable = true;
    programs.direnv.enableFishIntegration = true;
    programs.direnv.nix-direnv.enable = true;
  };
}
