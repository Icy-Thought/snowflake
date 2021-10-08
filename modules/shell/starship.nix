{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.htop;
in {
  options.modules.shell.htop.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    programs.starship.enable = true;
    programs.starship.enableBashIntegration = true;
    programs.starship.enableFishIntegration = true;
    programs.starship.enableZshIntegration = false;

    programs.starship.settings = {
      add_newline = true;
      scan_timeout = 10;

      character = {
        success_symbol = "[λ](bold green)";
        error_symbol = "[λ](bold red)";
      };

      battery = {
        full_symbol = "🔋";
        charging_symbol = "⚡️";
        discharging_symbol = "💀";
        display = [{
          style = "bold red";
          threshold = 15;
        }];
      };
    };
  };
}
