{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) map;
  inherit (lib) mkIf getExe;
in {
  config = mkIf (config.modules.shell.default == "fish") {
    modules.shell = {
      corePkgs.enable = true;
      toolset = {
        macchina.enable = true;
        starship.enable = true;
      };
    };

    # Enable starship-rs ZSH integration
    hm.programs.starship.enableFishIntegration = true;

    # Enables vendor completion: https://nixos.wiki/wiki/Fish
    programs.fish.enable = true;

    hm.programs.fish = {
      enable = true;
      # useBabelfish = true;

      shellAliases = {
        exa = "exa --group-directories-first";
        less = "less -R";
      };
      shellAbbrs = import "${config.snowflake.configDir}/shell-abbr";

      functions = {
        fish_title = "echo $argv[1]";
        sysdate = ''
          nixos-rebuild switch --use-remote-sudo --flake .#(hostname) --impure
        '';
      };

      interactiveShellInit = ''
        ${getExe pkgs.any-nix-shell} fish --info-right | source
        ${getExe pkgs.zoxide} init fish | source
        ${getExe pkgs.direnv} hook fish | source

        # Vi-Bindings:
        set fish_greeting
        set -g fish_key_bindings fish_vi_key_bindings

        # Color our Man-pages:
        set -xU LESS_TERMCAP_md (printf "\e[01;31m")
        set -xU LESS_TERMCAP_me (printf "\e[0m")
        set -xU LESS_TERMCAP_se (printf "\e[0m")
        set -xU LESS_TERMCAP_so (printf "\e[01;44;33m")
        set -xU LESS_TERMCAP_ue (printf "\e[0m")
        set -xU LESS_TERMCAP_us (printf "\e[01;32m"t)
      '';

      plugins = let
        mkPlugin = name: {
          inherit name;
          inherit (pkgs.fishPlugins."${name}") src;
        };
      in
        map (p: mkPlugin p) ["done" "autopair-fish" "fzf-fish"];
    };

    home.configFile = let
      inherit (config.modules.themes) active;
      inherit (config.modules.themes.colors.main) normal bright types;
    in (mkIf (active != null) {
      fish-fzf-theme = {
        target = "fish/conf.d/fzf.fish";
        text = ''
          set -Ux FZF_DEFAULT_OPTS " \
          --color=bg:,bg+:${types.bg},spinner:${types.panelbg},hl:${normal.red} \
          --color=fg:${types.border},header:${normal.red},info:${normal.magenta},pointer:${types.border} \
          --color=marker:${normal.magenta},fg+:${types.border},prompt:${types.border},hl+:${normal.red}"
        '';
      };

      fish-theme = {
        target = "fish/conf.d/${active}.fish";
        text = let
          inherit (config.modules.themes.colors) fishColor;
        in ''
          # --> General
          set -l foreground ${fishColor types.fg}
          set -l highlight  ${fishColor types.highlight}

          # --> palette
          set -l base01     ${fishColor normal.blue}
          set -l base02     ${fishColor types.border}
          set -l base03     ${fishColor bright.blue}
          set -l base04     ${fishColor bright.green}
          set -l base05     ${fishColor bright.red}
          set -l base06     ${fishColor types.panelbg}
          set -l base07     ${fishColor normal.yellow}
          set -l base08     ${fishColor normal.magenta}
          set -l base09     ${fishColor normal.cyan}
          set -l base10     ${fishColor bright.black}

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
      };
    });
  };
}
