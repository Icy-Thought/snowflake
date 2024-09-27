{ options, config, lib, pkgs, ... }:

let
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
  inherit (lib.options) mkEnableOption;

  cfg = config.modules.desktop.extensions.waybar;
in {
  options.modules.desktop.extensions.waybar = {
    enable = mkEnableOption "status-bar for wayland";
  };

  config = mkIf cfg.enable {
    hm.services.status-notifier-watcher.enable = true;

    hm.programs.waybar = let wayDir = "${config.snowflake.configDir}/waybar";
    in {
      enable = true;
      settings = builtins.fromJSON (builtins.readFile "${wayDir}/config");
      style = builtins.readFile "${wayDir}/style.css";
      systemd.enable = true;
    };
  };
}
