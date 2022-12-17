{ config
, options
, pkgs
, lib
, ...
}:
with lib;
with lib.my;

let
  cfg = config.modules.shell;
  themeCfg = config.modules.themes;
in
{
  config = mkIf (cfg.default == "zsh") {
    modules.shell.usefulPkgs.enable = true;

    # Custom shell modules:
    modules.shell.macchina.enable = true;
    modules.shell.xplr.enable = true;

    # Enable starship-rs:
    modules.shell.starship.enable = true;
    hm.programs.starship.enableZshIntegration = true;

    # Enable completion for sys-packages:
    environment.pathsToLink = [ "/share/zsh" ];

    hm.programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      autocd = true;
      enableSyntaxHighlighting = true;

      history = {
        size = 10000;
        path = "$XDG_CONFIG_HOME/zsh/history";
      };
      historySubstringSearch.enable = true;

      enableCompletion = true;
      initExtraBeforeCompInit = ''
        # -------===[ Uncategorized ]===------- #
        zstyle ':completion:*'                      completer _extensions _complete _approximate
        zstyle ':completion:*'                      accept-exact '*(N)'
        zstyle ':completion:*'                      file-sort modification
        zstyle ':completion:*'                      matcher-list ''' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
        zstyle ':completion:*'                      menu select=2
        zstyle ':completion:*'                      separate-sections 'yes'
        zstyle ':completion:*'                      squeeze-slashes true
        zstyle ':completion:*:default'              list-colors ''${(s.:.)LS_COLORS}
        zstyle ':completion::complete:*'            use-cache on
        zstyle ':completion:*'                      cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"

        # -------===[ Aesthetics ]===------- #
        zstyle ':completion:*:messages'             format ' %F{purple} -- %d --%f'
        zstyle ':completion:*:warnings'             format ' %F{red}-- no matches found --%f'
        zstyle ':completion:*:*:*:*:descriptions'   format '%F{green}-- %d --%f'
        zstyle ':completion:*:*:*:*:corrections'    format '%F{yellow}!- %d (errors: %e) -!%f'
      '';

      enableAutosuggestions = true;

      localVariables = {
        KEYTIMEOUT = "1";
        ZSH_AUTOSUGGEST_USE_ASYNC = true;
        ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE = 40;
        ZSH_AUTOSUGGEST_STRATEGY = [ "history" "completion" ];
      };

      initExtra = ''
        # -------===[ Uncategorized ]===------- #
        unsetopt BRACE_CCL                      # Brace character class list expansion.
        setopt COMBINING_CHARS                  # Zero-length punc chars + Base char.
        setopt RC_QUOTES                        # Allow single-quotes inside double-quotes.
        setopt HASH_LIST_ALL                    # Hash cmd-path on cmp || spell-correction.
        unsetopt CORRECT_ALL                    # Don't correct mis-spellings in args.
        unsetopt NOMATCH                        # Don't print err on no matches.
        unsetopt BEEP                           # Don't disturb the silence.
        setopt IGNORE_EOF                       # Don't exit on End-Of-File.
        WORDCHARS='_-*?[]~&.;!#$%^(){}<>'       # Special chars == part of a word!

        # -------===[ Historical ]===------- #
        setopt HIST_BEEP                        # Beep on non-existent history access.
        setopt HIST_EXPIRE_DUPS_FIRST           # Expire duplicate entries first.
        setopt HIST_FIND_NO_DUPS                # Don't display previously found entries.
        setopt HIST_IGNORE_ALL_DUPS             # No duplicate entries!
        setopt HIST_IGNORE_DUPS                 # Don't record entries twice in a row.
        setopt HIST_IGNORE_SPACE                # Don't record whitespace entries.
        setopt HIST_REDUCE_BLANKS               # Remove superfluous blanks before recording entry.
        setopt HIST_SAVE_NO_DUPS                # Don't write duplicate entires.
        setopt HIST_VERIFY                      # Don't execute on expansion!

        # -------===[ Terminal Jobs ]===------- #
        setopt LONG_LIST_JOBS                   # Long format job list.
        setopt NOTIFY                           # Report process status.
        unsetopt BG_NICE                        # Don't run all bg-jobs at a lower priority.
        unsetopt HUP                            # Don't kill jobs on shell exit.
        unsetopt CHECK_JOBS                     # Don't report on jobs on shell exit.

        # -------===[ Directories ]===------- #
        setopt AUTO_PUSHD                       # Push old dir -> stack on cd.
        setopt PUSHD_IGNORE_DUPS                # Don't store duplicates in stack.
        setopt PUSHD_SILENT                     # Don't print dir stack after pushd || popd.
        setopt PUSHD_TO_HOME                    # Push `~/.` when no argument is given.
        setopt CDABLE_VARS                      # Change directory -> path stored in var.
        setopt MULTIOS                          # Write -> multiple descriptors.
        setopt EXTENDED_GLOB                    # Use extended globbing syntax.
        unsetopt GLOB_DOTS
        unsetopt AUTO_NAME_DIRS                 # Don't add variable-stored paths to ~ list

        # -------===[ Aesthetics ]===------- #
        source "$HOME/.config/zsh/fzf.zsh"
        export MANPAGER="sh -c 'col -bx | bat -l man -p'"
        export ZSH_HIGHLIGHT_DIRS_BLACKLIST=(/nix/store)

        typeset -A ZSH_HIGHLIGHT_STYLES
        source "$HOME/.config/zsh/${themeCfg.active}.zsh"

        # -------===[ KeyBindings ]===------- #
        bindkey "^[[1;5C" forward-word                  # Ctrl-<R-arrow> -> move forward 1 word
        bindkey "^[[1;5D" backward-word                 # Ctrl-<L-arrow> -> move back 1 word
        bindkey '^[[Z' reverse-menu-complete            # Shift-Tab -> reverse menu navigation
        bindkey '^_' autosuggest-accept                 # C-/ => accept suggestion

        # -------===[ External Plugins ]===------- #
        any-nix-shell zsh --info-right | source /dev/stdin
        eval "$(zoxide init zsh)"
        eval "$(direnv hook zsh)"
      '';

      shellAliases = {
        exa = "exa --group-directories-first";
        less = "less -R";
      };

      plugins =
        let
          mkPlugin = name: {
            inherit name;
            inherit (pkgs."zsh-${name}") src;
          };
        in
        [{
          name = "zsh-abbr";
          src = pkgs.fetchFromGitHub {
            owner = "olets";
            repo = "zsh-abbr";
            rev = "v4.8.3";
            hash = "sha256-paCybqh6hJlDhIZKFEdypjpgbuPV4x3ezdtL4UZRoWw=";
          };
        }] ++ (builtins.map (p: mkPlugin p) [
          "autopair"
          "nix-shell"
          "vi-mode"
          "you-should-use"
        ]);
    };

    home.configFile = mkMerge [
      {
        zsh-abbreviations = {
          target = "zsh/abbreviations";
          text =
            let abbrevs = import "${config.snowflake.configDir}/shell-abbr";
            in ''
              ${concatStrings (mapAttrsToList (k: v: with strings; ''
                  abbr ${k}=${escapeNixString v}
              '') abbrevs)}
            '';
        };
      }

      (mkIf (themeCfg.active != null) {
        zsh-fzf-theme = {
          target = "zsh/fzf.zsh";
          text = with themeCfg.colors.main; ''
            export FZF_DEFAULT_OPTS=" \
            --color=bg:,bg+:${types.bg},spinner:${types.panelbg},hl:${normal.red} \
            --color=fg:${types.border},header:${normal.red},info:${normal.magenta},pointer:${types.border} \
            --color=marker:${normal.magenta},fg+:${types.border},prompt:${types.border},hl+:${normal.red}"
          '';
        };

        #  Reference: https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters/main.md
        zsh-theme = {
          target = "zsh/${themeCfg.active}.zsh";
          text = with themeCfg.colors.main; ''
            # -------===[ Comments ]===------- #
            ZSH_HIGHLIGHT_STYLES[comment]='fg=${normal.black}'

            # -------===[ Functions/Methods ]===------- #
            ZSH_HIGHLIGHT_STYLES[alias]='fg=${normal.magenta}'
            ZSH_HIGHLIGHT_STYLES[suffix-alias]='fg=${normal.magenta}'
            ZSH_HIGHLIGHT_STYLES[global-alias]='fg=${normal.magenta}'
            ZSH_HIGHLIGHT_STYLES[function]='fg=${normal.blue}'
            ZSH_HIGHLIGHT_STYLES[command]='fg=${normal.green}'
            ZSH_HIGHLIGHT_STYLES[precommand]='fg=${normal.green},italic'
            ZSH_HIGHLIGHT_STYLES[autodirectory]='fg=${normal.yellow},italic'
            ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=${normal.yellow}'
            ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=${normal.yellow}'
            ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg=${normal.magenta}'

            # -------===[ Built-ins ]===------- #
            ZSH_HIGHLIGHT_STYLES[builtin]='fg=${normal.blue}'
            ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=${normal.green}'
            ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=${normal.green}'

            # -------===[ Punctuation ]===------- #
            ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=${bright.red}'
            ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter]='fg=${types.border}'
            ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-unquoted]='fg=${types.border}'
            ZSH_HIGHLIGHT_STYLES[process-substitution-delimiter]='fg=${types.border}'
            ZSH_HIGHLIGHT_STYLES[back-quoted-argument-delimiter]='fg=${bright.red}'
            ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=${bright.red}'
            ZSH_HIGHLIGHT_STYLES[back-dollar-quoted-argument]='fg=${bright.red}'

            # -------===[ Strings ]===------- #
            ZSH_HIGHLIGHT_STYLES[command-substitution-quoted]='fg=${bright.yellow}'
            ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-quoted]='fg=${bright.yellow}'
            ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=${bright.yellow}'
            ZSH_HIGHLIGHT_STYLES[single-quoted-argument-unclosed]='fg=${normal.red}'
            ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=${bright.yellow}'
            ZSH_HIGHLIGHT_STYLES[double-quoted-argument-unclosed]='fg=${normal.red}'
            ZSH_HIGHLIGHT_STYLES[rc-quote]='fg=${bright.yellow}'

            # -------===[ Variables ]===------- #
            ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg=${types.highlight}'
            ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument-unclosed]='fg=${bright.red}'
            ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=${types.highlight}'
            ZSH_HIGHLIGHT_STYLES[assign]='fg=${types.highlight}'
            ZSH_HIGHLIGHT_STYLES[named-fd]='fg=${types.highlight}'
            ZSH_HIGHLIGHT_STYLES[numeric-fd]='fg=${types.highlight}'

            # -------===[ Non-Exclusive ]===------- #
            ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=${normal.red}'
            ZSH_HIGHLIGHT_STYLES[path]='fg=${types.highlight},underline'
            ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=${bright.red},underline'
            ZSH_HIGHLIGHT_STYLES[path_prefix]='fg=${types.highlight},underline'
            ZSH_HIGHLIGHT_STYLES[path_prefix_pathseparator]='fg=${bright.red},underline'
            ZSH_HIGHLIGHT_STYLES[globbing]='fg=${types.highlight}'
            ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=${normal.magenta}'
            ZSH_HIGHLIGHT_STYLES[back-quoted-argument-unclosed]='fg=${normal.red}'
            ZSH_HIGHLIGHT_STYLES[redirection]='fg=${types.highlight}'
            ZSH_HIGHLIGHT_STYLES[arg0]='fg=${types.highlight}'
            ZSH_HIGHLIGHT_STYLES[default]='fg=${types.highlight}'
            ZSH_HIGHLIGHT_STYLES[cursor]='fg=${types.highlight}'

             # -------===[ Patterns ]===------- #
            ZSH_HIGHLIGHT_PATTERNS+=('sudo ' 'fg=${types.fg},bold,bg=${normal.red}')
            ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=${types.fg},bold,bg=${normal.red}')

             # -------===[ Plugins ]===------- #
             # ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=${bright.black},bold,underline"
          '';
        };
      })
    ];
  };
}
