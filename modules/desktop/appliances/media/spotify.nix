{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.media.spotify;
in {
  options.modules.desktop.appliances.media.spotify = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ spotify ];
    # TODO: spicetify-cli + activeTheme.
  };
}
