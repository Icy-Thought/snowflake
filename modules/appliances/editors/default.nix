{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.appliances.editors;
in {
  options.modules.appliances.editors = { default = mkOpt types.str "emacs"; };

  config = mkIf (cfg.default != null) { env.EDITOR = cfg.default; };
}
