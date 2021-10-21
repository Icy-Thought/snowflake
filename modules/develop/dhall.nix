{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop.dhall;
in {
  options.modules.develop.dhall = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      haskellPackages.dhall
      haskellPackages.dhall-json
    ];
  };
}
