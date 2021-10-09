{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.defBrowser;
in {
  options.modules.desktop.defBrowser = {
    default = mkOpt (with types; nullOr str) null;
  };

  config = mkIf (cfg.default != null) { env.BROWSER = cfg.default; };
}
