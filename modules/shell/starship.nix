{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell;
in {
  options.modules.shell.starship = {
    enable = mkBoolOpt false;
  };

  config = mkIf (cfg.starship.enable || cfg.fish.enable) {
    home.programs.starship = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;

      settings = with config.modules.themes; {
        add_newline = true;
        scan_timeout = 10;

        format = "[$directory](fg:${colors.cyan}) $git_branch$git_status$character";
        directory = {
          style = "bg:${colors.cyan} fg:${colors.types.bg} bold";
          truncation_length = 3;
          truncation_symbol = "…/";
          format = "[⯁ $path]($style)";
        };

        git_branch = {
          style = "bg:${colors.green} fg:${colors.types.bg} bold";
          format = "[[](bg: ${colors.green})(on $symbol$branch)[](bg: ${colors.green})]($style) ";
          symbol = " ";
        };

        # TODO: find appealing symbols
        git_status = {
          style = "bg:${colors.red} fg:${colors.types.bg} bold";
          format = "[[](bg: ${colors.red})(「$all_status$ahead_behind」)[](bg: ${colors.red})]($style) ";
        };

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
