{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envManager.plasma;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.envManager.plasma = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.xserver.desktopManager.plasma5.enable = true;
    services.xserver.displayManager.sddm.enable = true;

    user.packages = with pkgs; [
      kdenlive
      orchis-theme
      flat-remix-gnome
      whitesur-icon-theme
    ];
  };
}
