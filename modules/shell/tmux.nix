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
        set -g status-style fg="${colors.white}",bg="${colors.black}",bold,italics

        set -g status-position top
        set -g status-justify left

        set -g status-left-length "40"
        set -g status-right-length "80"

        # Messages
        set -g message-style fg="${colors.black}",bg="${colors.magenta}",align="centre"
        set -g message-command-style fg="${colors.black}",bg="${colors.magenta}",align="centre"

        # Panes
        set -g pane-border-style fg="${colors.white}"
        set -g pane-active-border-style fg="${colors.blue}"

        # Windows
        set -g window-status-format "#[fg="${colors.white}"] #W/#{window_panes} "
        set -g window-status-current-format "#[fg="${colors.black}",bg="${colors.yellow}"]#{?client_prefix,#[fg="${colors.black}"],}#{?client_prefix,#[bg="${colors.magenta}"],} #W "

        # --------=== Status-line
        set -g status-left "🦊 "
        set -g status-bg "${colors.background}"
        set -g status-right "#[noitalics]#(set-volume status)  #(batStat)  #[noitalics,nobold]| %b %d, %H:%M:%S  #[fg="${colors.black}",bg="${colors.green}",bold,italics] #S "

        # --------=== Modes
        setw -g clock-mode-colour "${colors.cyan}"
        setw -g mode-style "fg=${colors.magenta} bg=${colors.black} bold"
      '';
    };
  };
}
