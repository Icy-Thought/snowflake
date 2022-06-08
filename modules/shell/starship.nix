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

        format = "[$directory](fg:#89dceb) ($git_branch)($git_status )($nix_shell)($character)";
        directory = {
          style = "bg:${colors.cyan} fg:${colors.types.bg} bold";
          truncation_length = 3;
          truncation_symbol = "…/";
          format = "[⯁ $path]($style)";
        };

        git_branch = {
          style = "bg:${colors.yellow} fg:${colors.types.bg} bold";
          format = "[[](bg: ${colors.yellow})(on $symbol$branch)[](bg: ${colors.yellow})]($style) ";
          symbol = " ";
        };

        # TODO: find appealing symbols
        git_status = {
          style = "bg:${colors.magenta} fg:${colors.types.bg} bold";
          format = "[([](bg: ${colors.magenta})「$all_status$ahead_behind」[](bg: ${colors.magenta}))]($style)";
        };

        character = {
          success_symbol = "[](bold green)";
          error_symbol = "[](bold red)";
          vicmd_symbol = "[](bold blue)";
        };

        nix_shell = {
          disabled = false;
          impure_msg = "[impure](bold red)";
          pure_msg = "[pure](bold green)";
          style = "bold blue";
          symbol = "[λ ](bold yellow)";
          format = "via [$symbol$state( \\($name\\))]($style) ";
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
