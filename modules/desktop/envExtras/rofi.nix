{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.rofi;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.rofi.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rofi-systemd ];

    programs.rofi.enable = true;
    programs.rofi.theme = "${configDir}/rofi/launcher/ribbon-left.rasi";
    programs.rofi.terminal = "${pkgs.kitty}/bin/kitty";

    programs.rofi.extraConfig = {
      font = "Iosevka 11";
      show-icons = true;
      icon-theme = "Whitesur-dark";
      display-drun = "λ";
      drun-display-format = "{name}";
      disable-history = false;
      sidebar-mode = false;
    };
  };
}
