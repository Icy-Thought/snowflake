{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.fish;
in {
  config = mkIf cfg.enable {
    homeManager.programs.starship = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;

      settings = {
        add_newline = true;
        scan_timeout = 10;

        character = {
          success_symbol = "[>>=](bold green)";
          error_symbol = "[>>=](bold red)";
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
  };
}
