{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.spotify;
in {
  options.modules.desktop.media.spotify = {enable = mkBoolOpt false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [spotify];
    # TODO: spicetify-cli + activeTheme.
  };
}
