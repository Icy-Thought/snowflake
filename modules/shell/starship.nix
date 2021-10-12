{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.starship;
in {
  options.modules.shell.starship = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    homeManager.starship = {
      starship.enable = true;
      starship.enableBashIntegration = true;
      starship.enableFishIntegration = true;
      starship.enableZshIntegration = false;

      starship.settings = {
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
  };
}
