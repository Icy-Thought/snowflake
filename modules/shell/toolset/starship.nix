{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf;
in {
  options.modules.shell.toolset.starship = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "minimal shell ricing";};

  config = mkIf config.modules.shell.toolset.starship.enable {
    hm.programs.starship = {
      enable = true;
      settings = let
        inherit (config.modules.themes.colors.main) normal types;
      in {
        scan_timeout = 10;
        add_newline = true;
        line_break.disabled = true;

        format = "[$directory](fg:${normal.yellow}) ($git_branch)($git_status )($nix_shell)\n($character)";
        # right_format = "[$cmd_duration](bg:none fg:${normal.magenta})";

        cmd_duration = {
          min_time = 1;
          format = "[ $duration]($style)";
          disabled = false;
          style = "bg:${normal.magenta} fg:${types.bg}";
        };

        directory = {
          format = "[  $path]($style)";
          style = "bg:${normal.yellow} fg:${types.bg} bold";
          truncation_length = 2;
          truncation_symbol = "…/";
        };

        git_branch = {
          format = "[[](fg:${types.border})( $branch)[](fg:${types.border})]($style) ";
          style = "bg:${types.border} fg:${types.bg} bold";
        };

        git_status = {
          format = "[([](fg:${normal.blue})( 『 $all_status$ahead_behind 』)[](fg:${normal.blue}))]($style)";
          style = "bg:${normal.blue} fg:${types.bg} bold";
        };

        character = {
          error_symbol = "[](${normal.red})";
          success_symbol = "[](${normal.green})";
          vicmd_symbol = "[](${normal.blue})";
        };

        nix_shell = {
          disabled = false;
          impure_msg = "[impure](${normal.red})";
          pure_msg = "[pure](${normal.green})";
          format = "via [$symbol$state( \\($name\\))]($style) ";
          style = "${normal.blue}";
          symbol = "[λ ](${normal.blue})";
        };

        battery = {
          full_symbol = "🔋";
          charging_symbol = "⚡️";
          discharging_symbol = "💀";
          display = [
            {
              style = "${normal.red}";
              threshold = 15;
            }
          ];
        };
      };
    };
  };
}
