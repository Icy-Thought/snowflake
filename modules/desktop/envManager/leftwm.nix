{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envManager.leftwm;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.envManager.leftwm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dunst
      polybar
      trayer
      feh
      shotgun
    ];

    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.windowManager.leftwm.enable = true;
    services.xserver.displayManager.defaultSession = "none+leftwm";

    xdg.configFile."leftwm/themes/garden/liquid" = {
      source = ./liquid; # TODO
      recursive = true;
    };

    xdg.configFile."leftwm/themes/garden" = {
      source = ./scripts;
      recursive = true;
    };

    xdg.configFile."leftwm/themes/garden/assets" = {
      source = ./assets;
      recursive = true;
    };
  };
}
