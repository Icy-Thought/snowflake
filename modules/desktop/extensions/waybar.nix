{ options, config, lib, pkgs, ... }:

let
  cfg = config.modules.desktop.extensions.waybar;
  wayDir = "${config.snowflake.configDir}/waybar";
in with lib; {
  options.modules.desktop.extensions.waybar = {
    enable = mkEnableOption "status-bar for wayland";
  };

  config = mkIf cfg.enable {
    hm.services.status-notifier-watcher.enable = true;

    hm.programs.waybar = {
      enable = true;
      settings = builtins.fromJSON (builtins.readFile "${wayDir}/config");
      style = builtins.readFile "${wayDir}/style.css";
      systemd.enable = true;
    };
  };
}
