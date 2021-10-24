{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.appliances.media.mpv;
in {
  options.modules.appliances.media.mpv = { enable = mkBoolOpt false; };

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ mpv-with-scripts mpvc ]; };
}
