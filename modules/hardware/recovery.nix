{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.hardware.recovery = {
    enable = mkEnableOption "HW recovery";
  };

  config = mkIf config.modules.hardware.recovery.enable {
    # TODO:
    # 1. disk recv. -> testdisk, ...
    # 2. auto-backup + btrfs snapshots or whatever
  };
}
