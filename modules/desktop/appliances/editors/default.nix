{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.editors;
in {
  options.modules.desktop.appliances.editors = { default = mkOpt types.str "emacs"; };

  config = mkIf (cfg.default != null) { env.EDITOR = cfg.default; };
}
