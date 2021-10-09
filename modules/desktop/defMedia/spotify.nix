{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.defMedia.spotify;
in {
  options.modules.desktop.defMedia.spotify = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ spotify ];
    # TODO: spicetify-cli + activeTheme.
  };
}
