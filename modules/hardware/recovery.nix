{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.hardware.recovery = { enable = mkBoolOpt false; };

  config = mkIf config.modules.hardware.recovery.enable {
    # TODO: 
    # 1. disk recv. -> testdisk, ...
    # 2. auto-backup + btrfs snapshots or whatever
  };
}
