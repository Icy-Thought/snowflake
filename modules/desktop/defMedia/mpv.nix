{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.defMedia.mpv;
in {
  options.modules.desktop.defMedia.mpv = { enable = mkBoolOpt false; };

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ mpv-with-scripts mpvc ]; };
}
