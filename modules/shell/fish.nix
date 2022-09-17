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
in {
  options.modules.shell.fish = {
    enable = mkBoolOpt false;
    theme = config.modules.themes;
  };

  config = mkIf cfg.fish.enable {
    modules.shell.usefulPkgs.enable = true;

    # Custom shell modules:
    modules.shell.macchina.enable = true;
    modules.shell.xplr.enable = true;

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

        # Vi-Bindings:
        set fish_greeting
        set -g fish_key_bindings fish_vi_key_bindings

        # Customizable `fish_title`:
        function fish_title
            echo $argv[1]
        end

        # Color our Man-pages:
        set -xU LESS_TERMCAP_md (printf "\e[01;31m")
        set -xU LESS_TERMCAP_me (printf "\e[0m")
        set -xU LESS_TERMCAP_se (printf "\e[0m")
        set -xU LESS_TERMCAP_so (printf "\e[01;44;33m")
        set -xU LESS_TERMCAP_ue (printf "\e[0m")
        set -xU LESS_TERMCAP_us (printf "\e[01;32m"t)

        ${builtins.readFile "${fishCfg}/abbreviations/main.fish"}
        ${builtins.readFile "${fishCfg}/aliases/main.fish"}
      '';

      plugins = let
        mkPlugin = name: {
          inherit name;
          inherit (pkgs.fishPlugins."${name}") src;
        };
      in
        builtins.map (p: mkPlugin p) [
          "done"
          "autopair-fish"
          "fzf-fish"
        ];
    };

    home.configFile = with config.modules.themes; (mkIf (active != null) {
      "fish/conf.d/fzf.fish".text = with colors.main; ''
        set -Ux FZF_DEFAULT_OPTS "\
        --color=bg:,bg+:${types.bg},spinner:${types.panelbg},hl:${normal.red} \
        --color=fg:${types.border},header:${normal.red},info:${normal.magenta},pointer:${types.border} \
        --color=marker:${normal.magenta},fg+:${types.border},prompt:${types.border},hl+:${normal.red}"
      '';

      "fish/conf.d/${config.modules.themes.active}.fish".text = with colors.fish; ''
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
