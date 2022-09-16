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
  # configDir = config.dotfiles.configDir;
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

      dotDir = "$XDG_CONFIG_HOME/zsh";

      history = {
        expireDuplicatesFirst = true;
        size = 10000;
        path = "$XDG_DATA_HOME/zsh/history";
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
        WORDCHARS='_-*?[]~&.;!#$%^(){}<>    # Special chars == part of a word!

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
          name = "zsh-fzf-tab";
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
  };
}
