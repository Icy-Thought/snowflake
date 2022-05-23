{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.fish;
in
{
  config = mkIf cfg.enable {
    homeManager.programs.starship = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;

      settings = with config.modules.themes; {
        add_newline = true;
        scan_timeout = 10;

        format = "[$directory](fg:${colors.cyan}) [$git_branch](fg:${colors.yellow}) [$git_status](fg:${colors.red}) $character ";
        directory.format = "[⯁ $path](bg:${colors.cyan} fg:${colors.types.bg} bold)";

        git_branch = {
          format = "[on $symbol$branch](bg:${colors.yellow} fg:${colors.types.bg} bold)";
          symbol = " ";
        };

        git_status.format = "[$all_status](bg:${colors.red} fg:${colors.types.bg} bold)";

        character = {
          success_symbol = "[](bold green)";
          error_symbol = "[](bold red)";
          vicmd_symbol = "[](bold blue)";
        };

        battery = {
          full_symbol = "🔋";
          charging_symbol = "⚡️";
          discharging_symbol = "💀";
          display = [
            {
              style = "bold red";
              threshold = 15;
            }
          ];
        };
      };
    };
  };
}
