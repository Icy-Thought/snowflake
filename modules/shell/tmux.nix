{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.tmux;
  term = config.modules.desktop.terminal;
in {
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;
  };

  config = mkIf (cfg.enable || term.alacritty.enable) {
    user.packages = with pkgs; [tmux];

    env = {
      PATH = ["$TMUXIFIER/bin"];
      TMUX_HOME = "$XDG_CONFIG_HOME/tmux";
    };

    modules.themes.onReload.tmux = "${getExe pkgs.tmux} source-file $TMUX_HOME/tmux.conf";

    home.configFile = {
      "fish/conf.d/tmux.fish".text = ''
        # Start Tmux on Fish start
        if status is-interactive && if ! set -q TMUX
            exec tmux
            end
        end
      '';

      "tmux/tmux.conf".text = with config.modules.themes; ''
        # --------=== General-Configurations
        set-option -g default-terminal "tmux-256color"
        set-option -g base-index 1
        set-window-option -g pane-base-index 1

        set-option -g status-keys vi
        set-option -g mode-keys vi

        # Rebind C-b -> C-a
        set-option -g prefix C-a
        unbind C-b
        bind-key C-a send-prefix

        # Disables confirmation on exit
        bind-key x kill-pane
        bind-key X kill-window
        bind-key q kill-session
        bind-key Q kill-server

        set-option  -g renumber-windows on
        set-window-option -g aggressive-resize off
        set-window-option -g automatic-rename on

        set-window-option -g clock-mode-style 24
        set-option -s escape-time 0
        set-option -g history-limit 5000

        set-option -g mouse on
        set-option -s focus-events on
        set-option -g renumber-windows on
        set-option -g allow-rename off

        # Activity/Sound
        set-option -g bell-action none
        set-option -g visual-bell off
        set-option -g visual-silence off
        set-option -g visual-activity off
        set-window-option -g monitor-activity off

        # --------=== Keybindings
        # Buffers
        bind-key b list-buffers
        bind-key p paste-buffer
        bind-key P choose-buffer

        # Split bindings
        bind-key / split-window -h -c '#{pane_current_path}'
        bind-key - split-window -v -c '#{pane_current_path}'
        bind-key c new-window -c '#{pane_current_path}'

        # Copy/Paste bindings
        bind-key P paste-buffer
        bind-key -T copy-mode-vi v send-keys -X begin-selection
        bind-key -T copy-mode-vi y send-keys -X copy-selection
        bind-key -T copy-mode-vi r send-keys -X rectangle-toggle

        # --------=== Status-bar
        set-option -g status on
        set-option -g status-interval 1
        set-option -g status-style bg=default,bold,italics

        set-option -g status-position top
        set-option -g status-justify left

        set-option -g status-left-length "40"
        set-option -g status-right-length "80"

        # Messages
        set-option -g message-style fg="${colors.types.bg}",bg="${colors.types.highlight}",align="centre"
        set-option -g message-command-style fg="${colors.types.bg}",bg="${colors.types.highlight}",align="centre"

        # Panes
        set-option -g pane-border-style fg="${colors.blue}"
        set-option -g pane-active-border-style fg="${colors.types.border}"

        # Windows
        set-option -g window-status-format "#[fg=${colors.white}] #W/#{window_panes} "
        set-option -g window-status-current-format "#[fg=${colors.types.bg},bg=${colors.types.border}]#{?client_prefix,#[fg=${colors.types.bg}],}#{?client_prefix,#[bg=${colors.blue}],} #W "

        # --------=== Status-line
        set-option -g status-left "🦊 "
        set-option -g status-bg default
        set-option -g status-right "#[italics]∡ #H | %b %d, %H:%M:%S  #[fg=${colors.types.bg},bg=${colors.types.panelbg},bold,italics] base-#S "

        # --------=== Clock & Selection
        set-window-option -g clock-mode-colour "${colors.types.border}"
        set-window-option -g mode-style "fg=${colors.types.bg} bg=${colors.types.highlight} bold"
      '';
    };
  };
}
