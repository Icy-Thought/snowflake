{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop.shell;
in {
  options.modules.develop.shell = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ shellcheck ]; };
}
