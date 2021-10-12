{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envManager.leftwm;
  configDir = config.snowflake.configDir;
  leftDir = config.snowflake.configDir/leftwm;
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

    services.xserver = {
      displayManager.lightdm.enable = true;
      windowManager.leftwm.enable = true;
      displayManager.defaultSession = "none+leftwm";
    };

    home.configFile = {
      "leftwm/config.toml".source = "${leftDir}/environment/config.toml";

      "leftwm/themes/garden/theme.toml".source =
        "${leftDir}/environment/theme.toml";

      "leftwm/default-apps.sh".source =
        "${leftDir}/environment/default-apps.sh";
    };

    home.configFile."leftwm/themes/garden/liquid" = {
      source = "${leftDir}/liquid";
      recursive = true;
    };

    home.configFile."leftwm/themes/garden" = {
      source = "${leftDir}/scripts";
      recursive = true;
    };

    home.configFile."leftwm/themes/garden/assets" = {
      source = "${leftDir}/assets";
      recursive = true;
    };
  };
}
