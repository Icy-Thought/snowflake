{ config
, options
, lib
, pkgs
, ...
}:

let inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in
{
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.shell.tmux.enable {
    hm.programs.tmux = {
      enable = true;
      secureSocket = true;
      keyMode = "vi";
      prefix = "C-a";
      terminal = "tmux-256color";

      baseIndex = 1;
      clock24 = true;
      disableConfirmationPrompt = true;
      escapeTime = 0;

      aggressiveResize = false;
      resizeAmount = 2;
      reverseSplit = false;
      historyLimit = 5000;
      newSession = true;

      plugins = with pkgs; [
        {
          plugin = tmuxPlugins.resurrect;
          extraConfig = "set -g @resurrect-strategy-nvim 'session'";
        }
        {
          plugin = tmuxPlugins.continuum;
          extraConfig = ''
            set -g @continuum-restore 'on'
            set -g @continuum-save-interval '60' # minutes
          '';
        }
      ];

      extraConfig = with config.modules.themes.colors.main; ''
        # -------===[ Color Correction ]===------- #
        set-option -ga terminal-overrides ",*256col*:Tc"
        set-option -ga terminal-overrides '*:Ss=\E[%p1%d q:Se=\E[ q'
        set-environment -g COLORTERM "truecolor"

        # -------===[ General-Configurations ]===------- #
        set-option -g renumber-windows on
        set-window-option -g automatic-rename on
        set-window-option -g word-separators ' @"=()[]'

        set-option -g mouse on
        set-option -s focus-events on
        set-option -g renumber-windows on
        set-option -g allow-rename off

        # -------===[ Activity/Sound ]===------- #
        set-option -g bell-action none
        set-option -g visual-bell off
        set-option -g visual-silence off
        set-option -g visual-activity off
        set-window-option -g monitor-activity off

        # -------===[ Keybindings ]===------- #
        bind-key Space clock-mode

        # Window Control(s):
        bind-key t new-window -c '#{pane_current_path}'
        bind-key q kill-session
        bind-key Q kill-server

        # Buffers:
        bind-key b list-buffers
        bind-key p paste-buffer
        bind-key P choose-buffer

        # Split bindings:
        bind-key / split-window -h -c '#{pane_current_path}'
        bind-key - split-window -v -c '#{pane_current_path}'

        # Copy/Paste bindings:
        bind-key -T copy-mode-vi v send-keys -X begin-selection     -N "Start visual mode for selection"
        bind-key -T copy-mode-vi y send-keys -X copy-selection      -N "Yank text into buffer"
        bind-key -T copy-mode-vi r send-keys -X rectangle-toggle    -N "Yank region into buffer"

        # -------===[ Status-Bar ]===------- #
        set-option -g status on
        set-option -g status-interval 1
        set-option -g status-style bg=default,bold,italics

        set-option -g status-position top
        set-option -g status-justify left

        set-option -g status-left-length "40"
        set-option -g status-right-length "80"

        # Messages:
        set-option -g message-style fg="${types.bg}",bg="${types.highlight}",align="centre"
        set-option -g message-command-style fg="${types.bg}",bg="${types.highlight}",align="centre"

        # Panes:
        set-option -g pane-border-style fg="${normal.blue}"
        set-option -g pane-active-border-style fg="${types.border}"

        # Windows:
        set-option -g window-status-format "#[fg=${normal.white}] #W/#{window_panes} "
        set-option -g window-status-current-format "#[fg=${types.bg},bg=${types.border}]#{?client_prefix,#[fg=${types.bg}],}#{?client_prefix,#[bg=${normal.blue}],} #W "

        # -------===[ Statusline ]===------- #
        set-option -g status-left "🌘 "
        set-option -g status-bg default
        set-option -g status-right "#[italics]∡ #H | %b %d, %H:%M  #[fg=${types.bg},bg=${types.fg},bold,italics] base-#S "

        # -------===[ Clock & Selection ]===------- #
        set-window-option -g clock-mode-colour "${types.border}"
        set-window-option -g mode-style "fg=${types.bg} bg=${types.highlight} bold"
      '';
    };
  };
}
