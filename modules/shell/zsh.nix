{
  config,
  options,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.zsh;
  zshDir = "${config.snowflake.configDir}/zsh";
in {
  options.modules.shell.zsh = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      any-nix-shell
      fzf
      pwgen
      yt-dlp
      csview

      # alternatives for several gnu-tools
      bottom
      exa
      fd
      (ripgrep.override {
        withPCRE2 = true;
      })
      zoxide
    ];

    # Custom shell modules:
    modules.shell.xplr.enable = true;
    modules.shell.macchina.enable = true;

    # Enable starship-rs:
    modules.shell.starship.enable = true;
    hm.programs.starship.enableZshIntegration = true;

    hm.programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;

      # Default config directory:
      dotDir = ".config/zsh";

      history = {
        size = 10000;
        expireDuplicatesFirst = true;
      };
      historySubstringSearch.enable = true;

      initExtra = ''
        # -------===[ General ]===------- #
        unsetopt BRACE_CCL                  # Brace character class list expansion.
        setopt COMBINING_CHARS              # Zero-length punc chars + Base char.
        setopt RC_QUOTES                    # Allow single-quotes inside double-quotes.
        setopt HASH_LIST_ALL                # Hash cmd-path on cmp || spell-correction.
        unsetopt CORRECT_ALL                # Don't correct mis-spellings in args.
        unsetopt NOMATCH                    # Don't print err on no matches.
        unsetopt BEEP                       # Don't disturb the silence.
        setopt HIST_BEEP                    # Beep on non-existent history access.
        setopt IGNORE_EOF                   # Don't exit on End-Of-File.
        WORDCHARS='_-*?[]~&.;!#$%^(){}<>'   # Special chars == part of a word!

        # -------===[ Jobs ]===------- #
        setopt LONG_LIST_JOBS               # Long format job list.
        setopt NOTIFY                       # Report process status.
        unsetopt BG_NICE                    # Don't run all bg-jobs at a lower priority.
        unsetopt HUP                        # Don't kill jobs on shell exit.
        unsetopt CHECK_JOBS                 # Don't report on jobs on shell exit.

        # -------===[ Directories ]===------- #
        setopt AUTO_PUSHD                   # Push old dir -> stack on cd.
        setopt PUSHD_IGNORE_DUPS            # Don't store duplicates in stack.
        setopt PUSHD_SILENT                 # Don't print dir stack after pushd || popd.
        setopt PUSHD_TO_HOME                # Push `~/.` when no argument is given.
        setopt CDABLE_VARS                  # Change directory -> path stored in var.
        setopt MULTIOS                      # Write -> multiple descriptors.
        setopt EXTENDED_GLOB                # Use extended globbing syntax.
        unsetopt GLOB_DOTS
        unsetopt AUTO_NAME_DIRS             # Don't add variable-stored paths to ~ list

        # -------===[ Aliases ]===------- #
        alias exa="exa --group-directories-first"

        # -------===[ External Plugins ]===------- #
        eval "$(zoxide init zsh)"
      '';

      plugins = with pkgs; [
        {
          name = "zsh-nix-shell";
          src = zsh-nix-shell.src;
        }
        {
          name = "zsh-vi-mode";
          src = zsh-vi-mode.src;
        }
        {
          name = "zsh-fzf-tab"; # TODO: Source before auto-suggestion
          src = zsh-fzf-tab.src;
        }
        {
          name = "zsh-autopair";
          src = zsh-autopair.src;
        }
        {
          name = "zsh-you-should-use";
          src = zsh-you-should-use.src;
        }
        {
          name = "zsh-abbr";
          src = pkgs.fetchFromGitHub {
            owner = "olets";
            repo = "zsh-abbr";
            rev = "v4.8.0";
            sha256 = "diitszKbu530zXbJx4xmfOjLsITE9ucmWdsz9VTXsKg=";
          };
        }
      ];
    };

    home.configFile = let
      themeCfg = config.modules.themes;
    in
      mkMerge [
        {
          "zsh/abbreviations".text = ''
            ${builtins.readFile "${zshDir}/abbreviations/main.zsh"}
            ${builtins.readFile "${zshDir}/abbreviations/git.zsh"}
          '';
        }

        (mkIf (themeCfg.active != null) {
          "zsh/${themeCfg.active}.zsh".text = with themeCfg.colors.main; ''
            # -------===[ Reference ]===------- #
            #  Reference: https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters/main.md

            # -------===[ Comments ]===------- #
            ZSH_HIGHLIGHT_STYLES[comment]='fg=${normal.black}'

            # -------===[ Functions/Methods ]===------- #
            ZSH_HIGHLIGHT_STYLES[alias]='fg=${normal.green}'
            ZSH_HIGHLIGHT_STYLES[suffix-alias]='fg=${normal.green}'
            ZSH_HIGHLIGHT_STYLES[global-alias]='fg=${normal.green}'
            ZSH_HIGHLIGHT_STYLES[function]='fg=${normal.green}'
            ZSH_HIGHLIGHT_STYLES[command]='fg=${normal.green}'
            ZSH_HIGHLIGHT_STYLES[precommand]='fg=${normal.green},italic'
            ZSH_HIGHLIGHT_STYLES[autodirectory]='fg=${normal.yellow},italic'
            ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=${normal.yellow}'
            ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=${normal.yellow}'
            ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg=${normal.magenta}'

            # -------===[ Built-ins ]===------- #
            ZSH_HIGHLIGHT_STYLES[builtin]='fg=${normal.green}'
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
            ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg=${bright.cyan}'
            ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument-unclosed]='fg=${bright.red}'
            ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=${bright.cyan}'
            ZSH_HIGHLIGHT_STYLES[assign]='fg=${bright.cyan}'
            ZSH_HIGHLIGHT_STYLES[named-fd]='fg=${bright.cyan}'
            ZSH_HIGHLIGHT_STYLES[numeric-fd]='fg=${bright.cyan}'


            # -------===[ Non-Exclusive ]===------- #
            ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=${normal.red}'
            ZSH_HIGHLIGHT_STYLES[path]='fg=${bright.cyan},underline'
            ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=${bright.red},underline'
            ZSH_HIGHLIGHT_STYLES[path_prefix]='fg=${bright.cyan},underline'
            ZSH_HIGHLIGHT_STYLES[path_prefix_pathseparator]='fg=${bright.red},underline'
            ZSH_HIGHLIGHT_STYLES[globbing]='fg=${bright.cyan}'
            ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=${normal.magenta}'
            ZSH_HIGHLIGHT_STYLES[back-quoted-argument-unclosed]='fg=${normal.red}'
            ZSH_HIGHLIGHT_STYLES[redirection]='fg=${bright.cyan}'
            ZSH_HIGHLIGHT_STYLES[arg0]='fg=${bright.cyan}'
            ZSH_HIGHLIGHT_STYLES[default]='fg=${bright.cyan}'
            ZSH_HIGHLIGHT_STYLES[cursor]='fg=${bright.cyan}'

            # -------===[ Plugins ]===------- #
            ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=${bright.black},bg=${normal.cyan},bold,underline"
          '';
        })
      ];
  };
}
