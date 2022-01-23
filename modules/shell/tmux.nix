{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.tmux;
  colors = config.modules.themes.colors;
in {
  options.modules.shell.tmux = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    homeManager.programs.tmux = {
      enable = true;
      secureSocket = true;
      prefix = "C-a";
      terminal = "screen-256color";

      baseIndex = 1;
      escapeTime = 0;
      keyMode = "vi";
      disableConfirmationPrompt = true;

      clock24 = true;
      resizeAmount = 2;
      historyLimit = 5000;

      extraConfig = ''
        # --------=== General-Configurations
        set -g mouse on
        set -s focus-events on
        set -g renumber-windows on
        set-option -g allow-rename off

        # Automatic Term-Window Title Change
        set -g set-titles on
        set -g set-titles-string 'tmux - #S'
        setw -g automatic-rename

        # Activity/Sound
        set -g visual-activity off
        set -g visual-bell off
        set -g visual-silence off
        setw -g monitor-activity off
        set -g bell-action none

        # Buffers
        bind b list-buffers
        bind p paste-buffer
        bind P choose-buffer

        # Split bindings
        bind / split-window -h -c '#{pane_current_path}'
        bind - split-window -v -c '#{pane_current_path}'
        bind c new-window -c '#{pane_current_path}'

        # --------=== Status-bar
        set -g status on
        set -g status-interval 1
        set -g status-position top
        set -g status-justify left

        set -g status-left-length "100"
        set -g status-right-length "100"

        # Messages
        set -g message-style fg="${colors.cyan}",bg="${colors.white}",align="centre"
        set -g message-command-style fg="${colors.cyan}",bg="${colors.white}",align="centre"

        # Panes
        set -g pane-border-style fg="${colors.white}"
        set -g pane-active-border-style fg="${colors.blue}"

        # Windows
        set -g window-status-format "#[fg=${colors.foreground}] #W#[noitalics,nobold]|#{window_panes}#[italics,bold] "
        set -g window-status-current-format "#[bg=${colors.background}]#{?client_prefix,#[fg=${colors.foreground}],}#{?client_prefix,#[bg=${colors.background}],} #W#[noitalics,nobold]|#{window_panes}#[italics,bold] "

        # --------=== Status-line
        set -g status-left '🦊'
        set -g status-bg "${colors.background}"
        set -g status-right "#[noitalics]#(set-volume status)  #(batStat)  #[noitalics,nobold]| %b %d, %H:%M:%S  #[fg=${colors.foreground},bg=${colors.background},bold,italics] #S "

        # --------=== Modes
        setw -g clock-mode-colour "${colors.blue}"
        setw -g mode-style "fg=${colors.magenta} bg=${colors.black} bold"
      '';
    };
  };
}
