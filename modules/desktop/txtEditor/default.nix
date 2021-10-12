{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.txtEditor;
in {
  options.modules.desktop.txtEditor = { default = mkOpt types.str "emacs"; };

  config = mkIf (cfg.default != null) { env.EDITOR = cfg.default; };
}
