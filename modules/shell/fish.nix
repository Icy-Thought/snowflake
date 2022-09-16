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
  fishCfg = "${config.snowflake.configDir}/fish";
  themeCfg = config.modules.themes;
in {
  options.modules.shell.fish = {
    enable = mkBoolOpt false;
    theme = config.modules.themes;
  };

  config = mkIf cfg.fish.enable {
    # Custom shell modules:
    modules.shell.xplr.enable = true;
    modules.shell.macchina.enable = true;

    # Enable starship-rs:
    modules.shell.starship.enable = true;
    hm.programs.starship.enableFishIntegration = true;

    # Enables vendor completion: https://nixos.wiki/wiki/Fish
    programs.fish.enable = true;

    hm.programs.fish = {
      enable = true;
      # useBabelfish = true;
      interactiveShellInit = ''
        ${getExe pkgs.starship} init fish | source
        ${getExe pkgs.zoxide} init fish | source
        ${getExe pkgs.any-nix-shell} fish | source

        ${builtins.readFile "${fishCfg}/interactive.fish"}
        ${builtins.readFile "${fishCfg}/abbreviations/main.fish"}
        ${builtins.readFile "${fishCfg}/aliases/main.fish"}
      '';

      plugins = with pkgs.fishPlugins; [
        {
          name = "done";
          src = done.src;
        }
        {
          name = "autopair-fish";
          src = autopair-fish.src;
        }
        {
          name = "fzf-fish";
          src = fzf-fish.src;
        }
      ];
    };

    home.configFile = with themeCfg; (mkIf (themeCfg.active != null) {
      "fish/conf.d/fzf.fish".text = with colors.main; ''
        set -Ux FZF_DEFAULT_OPTS "\
        --color=bg+:${normal.black},bg:${types.bg},spinner:${types.highlight},hl:${normal.red} \
        --color=fg:${types.border},header:${normal.red},info:${normal.magenta},pointer:${types.highlight} \
        --color=marker:${types.highlight},fg+:${types.border},prompt:${normal.magenta},hl+:${normal.red}"
      '';

      "fish/conf.d/${themeCfg.active}.fish".text = with colors.fish; ''
        # --> General
        set -l foreground ${fg}
        set -l highlight  ${highlight}

        # --> palette
        set -l base01     ${base01}
        set -l base02     ${base02}
        set -l base03     ${base03}
        set -l base04     ${base04}
        set -l base05     ${base05}
        set -l base06     ${base06}
        set -l base07     ${base07}
        set -l base08     ${base08}
        set -l base09     ${base09}
        set -l base10     ${base10}

        # Syntax Highlighting
        set -g fish_color_normal                     $foreground
        set -g fish_color_command                    $base09
        set -g fish_color_param                      $base02
        set -g fish_color_keyword                    $base08
        set -g fish_color_quote                      $base07
        set -g fish_color_redirection                $foreground
        set -g fish_color_end                        $base06
        set -g fish_color_error                      $base05
        set -g fish_color_gray                       $base10
        set -g fish_color_selection     --background=$highlight
        set -g fish_color_search_match  --background=$highlight
        set -g fish_color_operator                   $base04
        set -g fish_color_escape                     $base08
        set -g fish_color_autosuggestion             $base10
        set -g fish_color_cancel                     $base05

        # Prompt
        set -g fish_color_cwd                        $base08
        set -g fish_color_user                       $base01
        set -g fish_color_host                       $base09

        # Completion Pager
        set -g fish_pager_color_progress             $base10
        set -g fish_pager_color_prefix               $base07
        set -g fish_pager_color_completion           $foreground
        set -g fish_pager_color_description          $base10
      '';
    });
  };
}
