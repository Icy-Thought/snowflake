{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.fish;
in {
  config = mkIf cfg.enable {
    homeManager.programs.tmux = {
      enable = true;
      secureSocket = true;
      prefix = "C-a";
      terminal = "tmux-256color";

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

        # Activity/Sound
        set -g bell-action none
        set -g visual-bell off
        set -g visual-silence off
        set -g visual-activity off
        setw -g monitor-activity off

        # --------=== Keybindings
        # Buffers
        bind b list-buffers
        bind p paste-buffer
        bind P choose-buffer

        # Split bindings
        bind / split-window -h -c '#{pane_current_path}'
        bind - split-window -v -c '#{pane_current_path}'
        bind c new-window -c '#{pane_current_path}'

        # Copy/Paste bindings
        bind P paste-buffer
        bind -T copy-mode-vi v send-keys -X begin-selection
        bind -T copy-mode-vi y send-keys -X copy-selection
        bind -T copy-mode-vi r send-keys -X rectangle-toggle

        # --------=== Status-bar
        set -g status on
        set -g status-interval 1
        set -g status-style fg="colour7",bg="colour0",bold,italics

        set -g status-position top
        set -g status-justify left

        set -g status-left-length "40"
        set -g status-right-length "80"

        # Messages
        set -g message-style fg=colour0,bg=colour5,align="centre"
        set -g message-command-style fg=colour0,bg=colour5,align="centre"

        # Panes
        set -g pane-border-style fg=colour7
        set -g pane-active-border-style fg=colour4

        # Windows
        set -g window-status-format "#[fg=colour7] #W/#{window_panes} "
        set -g window-status-current-format "#[fg=colour0,bg=colour3]#{?client_prefix,#[fg=colour0],}#{?client_prefix,#[bg=colour5],} #W "

        # --------=== Status-line
        set -g status-left "🦊 "
        set -g status-bg default
        set -g status-right "#[noitalics]#(set-volume status)  #(batStat)  #[noitalics,nobold]| %b %d, %H:%M:%S  #[fg=colour0,bg=colour2,bold,italics] #S "

        # --------=== Modes
        setw -g clock-mode-colour colour6
        setw -g mode-style "fg=colour5 bg=colour0 bold"
      '';
    };
  };
}
