{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.appliances.philomath.electroGears;
in {
  options.modules.appliances.philomath.electroGears = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # TODO: OSS packages + configuration.
    user.packages = with pkgs; [ kicad ];
  };
}
