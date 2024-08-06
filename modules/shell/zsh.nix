{
  config,
  options,
  pkgs,
  lib,
  ...
}: let
  inherit (lib.attrsets) mapAttrsToList;
  inherit (lib.modules) mkIf;
  inherit (lib.strings) concatStrings escapeNixString;

  cfg = config.modules.shell;
in {
  config = mkIf (cfg.default == "zsh") {
    modules.shell = {
      corePkgs.enable = true;
      toolset.starship.enable = true;
    };

    hm.programs.starship.enableZshIntegration = true;

    # Enable completion for sys-packages:
    environment.pathsToLink = ["/share/zsh"];

    programs.zsh.enable = true;

    hm.programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      autocd = true;
      defaultKeymap = "viins";

      history = {
        size = 10000;
        path = "$XDG_CONFIG_HOME/zsh/history";
      };

      historySubstringSearch = {
        enable = true;
        searchUpKey = "^[OA";
        searchDownKey = "^[OB";
      };

      enableCompletion = true;
      completionInit = ''
        # -------===[ General ]===------- #
        zstyle ':completion:*'                      completer _extensions _complete _approximate
        zstyle ':completion::complete:*'            use-cache on
        zstyle ':completion:*'                      cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"
        zstyle ':completion:*'                      accept-exact '*(N)'
        zstyle ':completion:*'                      file-sort modification
        zstyle ':completion:*'                      matcher-list ''' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
        zstyle ':completion:*'                      group-name '''
        zstyle ':completion:*'                      menu select=2
        zstyle ':completion:*'                      separate-sections 'yes'
        zstyle ':completion:*'                      squeeze-slashes true

        # -------===[ Aesthetics ]===------- #
        zstyle ':completion:*:default'              list-colors ''${(s.:.)LS_COLORS}
        zstyle ':completion:*:commands'             list-colors '=*=1;32'
        zstyle ':completion:*:messages'             format ' %F{purple} -- %d --%f'
        zstyle ':completion:*:warnings'             format ' %F{red}-- no matches found --%f'
        zstyle ':completion:*:*:*:*:descriptions'   format '%F{green}-- %d --%f'
        zstyle ':completion:*:*:*:*:corrections'    format '%F{yellow}!- %d (errors: %e) -!%f'
      '';
      autosuggestion.enable = true;

      initExtra = ''
        # -------===[ Uncategorized ]===------- #
        unsetopt BRACE_CCL                       # Brace character class list expansion.
        setopt COMBINING_CHARS                   # Zero-length punc chars + Base char.
        setopt RC_QUOTES                         # Allow single-quotes inside double-quotes.
        setopt HASH_LIST_ALL                     # Hash cmd-path on cmp || spell-correction.
        unsetopt CORRECT_ALL                     # Don't correct mis-spellings in args.
        unsetopt NOMATCH                         # Don't print err on no matches.
        unsetopt BEEP                            # Don't disturb the silence.
        setopt IGNORE_EOF                        # Don't exit on End-Of-File.
        export WORDCHARS='_-*?[]~&.;!#$%^(){}<>' # Special chars == part of a word!

        # -------===[ Historical ]===------- #
        setopt HIST_BEEP                         # Beep on non-existent history access.
        setopt HIST_EXPIRE_DUPS_FIRST            # Expire duplicate entries first.
        setopt HIST_FIND_NO_DUPS                 # Don't display previously found entries.
        setopt HIST_IGNORE_ALL_DUPS              # No duplicate entries!
        setopt HIST_IGNORE_DUPS                  # Don't record entries twice in a row.
        setopt HIST_IGNORE_SPACE                 # Don't record whitespace entries.
        setopt HIST_REDUCE_BLANKS                # Remove superfluous blanks before recording entry.
        setopt HIST_SAVE_NO_DUPS                 # Don't write duplicate entires.
        setopt HIST_VERIFY                       # Don't execute on expansion!

        # -------===[ Terminal Jobs ]===------- #
        setopt LONG_LIST_JOBS                    # Long format job list.
        setopt NOTIFY                            # Report process status.
        unsetopt BG_NICE                         # Don't run all bg-jobs at a lower priority.
        unsetopt HUP                             # Don't kill jobs on shell exit.
        unsetopt CHECK_JOBS                      # Don't report on jobs on shell exit.

        # -------===[ Directories ]=== ------- #
        setopt AUTO_PUSHD                        # Push old dir -> stack on cd.
        setopt PUSHD_IGNORE_DUPS                 # Don't store duplicates in stack.
        setopt PUSHD_SILENT                      # Don't print dir stack after pushd || popd.
        setopt PUSHD_TO_HOME                     # Push `~/.` when no argument is given.
        setopt CDABLE_VARS                       # Change directory -> path stored in var.
        setopt MULTIOS                           # Write -> multiple descriptors.
        setopt EXTENDED_GLOB                     # Use extended globbing syntax.
        unsetopt GLOB_DOTS
        unsetopt AUTO_NAME_DIRS                  # Don't add variable-stored paths to ~ list

        # -------===[ Aesthetics ]===------- #
        export MANPAGER="sh -c 'col -bx | bat -l man -p'"
        export MANROFFOPT="-c"
        export ZSH_HIGHLIGHT_DIRS_BLACKLIST=(/nix/store)

        # -------===[ KeyBindings ]===------- #
        bindkey "^[[1;5D" backward-word          # Ctrl-<L-arrow> -> move back 1 word
        bindkey "^[[1;5C" forward-word           # Ctrl-<R-arrow> -> move forward 1 word
        bindkey "^[[Z" reverse-menu-complete     # Shift-Tab -> reverse menu navigation
        bindkey "^_" autosuggest-accept          # C-/ => accept suggestion

        # -------===[ Useful Functions ]===------- #
        function sysdate {
            nixos-rebuild switch --use-remote-sudo --flake .#"$(hostname)" --impure
        }

        # -------===[ External Plugins ]===------- #
        ZSH_AUTOSUGGEST_STRATEGY=(completion history)

        any-nix-shell zsh | source /dev/stdin
        eval "$(zoxide init zsh)"
        eval "$(direnv hook zsh)"
      '';

      shellAliases = {
        less = "less -R";
      };

      plugins = let
        mkZshPlugin = {
          pkg,
          file ? "${pkg.pname}.plugin.zsh",
        }: {
          name = pkg.pname;
          src = pkg.src;
          inherit file;
        };
      in
        with pkgs; [
          (mkZshPlugin {pkg = zsh-abbr;})
          (mkZshPlugin {pkg = zsh-autopair;})
          (mkZshPlugin {pkg = zsh-you-should-use;})
          (mkZshPlugin {
            pkg = zsh-nix-shell;
            file = "nix-shell.plugin.zsh";
          })
        ];

      syntaxHighlighting = let
        inherit (config.modules.themes) active;
        inherit (config.modules.themes.colors.main) normal bright types;
      in
        mkIf (active != null) {
          enable = true;
          highlighters = ["main" "brackets" "pattern" "cursor" "regexp" "root" "line"];
          patterns = {
            "sudo " = "fg=${normal.red},bold";
            "rm -rf *" = "fg=${normal.red},bold";
          };
          styles = {
            # -------===[ Comments ]===------- #
            comment = "fg=${normal.black}";

            # -------===[ Functions/Methods ]===------- #
            alias = "fg=${normal.magenta}";
            "suffix-alias" = "fg=${normal.magenta}";
            "global-alias" = "fg=${normal.magenta}";
            function = "fg=${normal.blue}";
            command = "fg=${normal.green}";
            precommand = "fg=${normal.green},italic";
            autodirectory = "fg=${normal.yellow},italic";
            "single-hyphen-option" = "fg=${normal.yellow}";
            "double-hyphen-option" = "fg=${normal.yellow}";
            "back-quoted-argument" = "fg=${normal.magenta}";

            # -------===[ Built-ins ]===------- #
            builtin = "fg=${normal.blue}";
            "reserved-word" = "fg=${normal.green}";
            "hashed-command" = "fg=${normal.green}";

            # -------===[ Punctuation ]===------- #
            commandseparator = "fg=${bright.red}";
            "command-substitution-delimiter" = "fg=${types.border}";
            "command-substitution-delimiter-unquoted" = "fg=${types.border}";
            "process-substitution-delimiter" = "fg=${types.border}";
            "back-quoted-argument-delimiter" = "fg=${bright.red}";
            "back-double-quoted-argument" = "fg=${bright.red}";
            "back-dollar-quoted-argument" = "fg=${bright.red}";

            # -------===[ Strings ]===------- #
            "command-substitution-quoted" = "fg=${bright.yellow}";
            "command-substitution-delimiter-quoted" = "fg=${bright.yellow}";
            "single-quoted-argument" = "fg=${bright.yellow}";
            "single-quoted-argument-unclosed" = "fg=${normal.red}";
            "double-quoted-argument" = "fg=${bright.yellow}";
            "double-quoted-argument-unclosed" = "fg=${normal.red}";
            "rc-quote" = "fg=${bright.yellow}";

            # -------===[ Variables ]===------- #
            "dollar-quoted-argument" = "fg=${types.highlight}";
            "dollar-quoted-argument-unclosed" = "fg=${bright.red}";
            "dollar-double-quoted-argument" = "fg=${types.highlight}";
            assign = "fg=${types.highlight}";
            "named-fd" = "fg=${types.highlight}";
            "numeric-fd" = "fg=${types.highlight}";

            # -------===[ Non-Exclusive ]===------- #
            "unknown-token" = "fg=${normal.red}";
            path = "fg=${types.highlight},underline";
            path_pathseparator = "fg=${bright.red},underline";
            path_prefix = "fg=${types.highlight},underline";
            path_prefix_pathseparator = "fg=${bright.red},underline";
            globbing = "fg=${types.highlight}";
            "history-expansion" = "fg=${normal.magenta}";
            "back-quoted-argument-unclosed" = "fg=${normal.red}";
            redirection = "fg=${types.highlight}";
            arg0 = "fg=${types.highlight}";
            default = "fg=${types.highlight}";
            cursor = "fg=${types.highlight}";
          };
        };
    };

    create.configFile.zsh-abbreviations = {
      target = "zsh/abbreviations";
      text = let
        abbrevs = import "${config.snowflake.configDir}/shell-abbr.nix";
      in ''
        ${concatStrings (mapAttrsToList (k: v: ''
            abbr ${k}=${escapeNixString v}
          '')
          abbrevs)}
      '';
    };
  };
}
