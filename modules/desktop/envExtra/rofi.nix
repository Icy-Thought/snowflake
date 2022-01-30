{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envExtra.rofi;
  configDir = config.snowflake.configDir;
  font = config.modules.fonts.settings;
in {
  options.modules.desktop.envExtra.rofi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rofi-systemd ];

    # TODO: proper theme + options after styled properly.. current != good.
    homeManager.programs.rofi = {
      enable = true;
      theme = "${configDir}/rofi/launcher/ribbon-left.rasi";
      terminal = "${pkgs.alacritty}/bin/alacritty";

      extraConfig = {
        font = "${font.family} ${font.style} 10";
        show-icons = true;
        icon-theme = "Whitesur-dark";
        display-drun = "λ";
        drun-display-format = "{name}";
        disable-history = false;
        sidebar-mode = false;
      };
    };
  };
}
