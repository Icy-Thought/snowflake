{ config, options, lib, pkgs, ... }:
let inherit (lib.modules) mkIf;
in {
  options.modules.hardware.recovery = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "HW recovery"; };

  config = mkIf config.modules.hardware.recovery.enable {
    # TODO:
    # 1. disk recv. -> testdisk, ...
    # 2. auto-backup + btrfs snapshots or whatever
  };
}
