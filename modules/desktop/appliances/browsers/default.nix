{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.browsers;
in {
  options.modules.desktop.appliances.browsers = {
    default = mkOpt (with types; nullOr str) null;
  };

  config = mkIf (cfg.default != null) { env.BROWSER = cfg.default; };
}
