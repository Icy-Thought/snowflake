{ options, config, lib, pkgs, ... }:

let activeTheme = config.modules.themes.active;
in with lib; {
  options.modules.desktop.terminal.kitty = {
    enable = mkEnableOption "GPU-accelerated terminal emulator";
  };

  config = mkIf config.modules.desktop.terminal.kitty.enable {
    hm.programs.kitty = {
      enable = true;
      settings = {
        term = "xterm-kitty";
        repaint_delay = 10;
        update_check_interval = 0;

        sync_to_monitor = "yes";
        allow_remote_control = "no";
        close_on_child_death = "no";
        shell_integration = "no-cursor";
        confirm_os_window_close = 0;

        background_opacity = "0.8";
        inactive_text_alpha = "1.0";
        disable_ligatures = "cursor";
        modify_font = builtins.concatStringsSep " "
          (mapAttrsToList (name: value: "${name} ${value}") {
            cell_height = "110%";
          });

        enable_audio_bell = "no";
        bell_on_tab = "no";
        visual_bell_duration = 0;

        strip_trailing_spaces = "smart";
        copy_on_select = "clipboard";
        select_by_word_characters = "@-./_~?&=%+#";
        clipboard_control = "write-clipboard write-primary no-append";

        default_pointer_shape = "beam";
        cursor_shape = "block";
        cursor_blink_interval = 0;

        input_delay = 3;
        pointer_shape_when_dragging = "beam";
        pointer_shape_when_grabbed = "arrow";

        click_interval = "0.5";
        mouse_hide_wait = 3;
        focus_follows_mouse = "yes";

        detect_urls = "yes";
        open_url_with = "default";
        url_prefixes = "http https file ftp gemini irc gopher mailto news git";

        scrollback_lines = 5000;
        wheel_scroll_multiplier = 5;

        draw_minimal_borders = "yes";
        placement_strategy = "top-left";
        window_border_width = 1;
        window_margin_width = 0;
        window_padding_width = 10;

        active_tab_font_style = "normal";
        inactive_tab_font_style = "normal";
        tab_bar_style = "custom";
        tab_bar_edge = "top";
        tab_bar_margin_height = "0.0 0.0";
        tab_bar_min_tabs = 1;
      };

      keybindings = {
        "ctrl+shift+end" = "load_config_file";
        "ctrl+shift+0" = "restore_font_size";
        "middle release ungrabbed" = "paste_from_selection";

        "ctrl+shift+t" = "new_tab_with_cwd";
        "ctrl+shift+j" = "next_tab";
        "ctrl+shift+page_up" = "next_tab";
        "ctrl+shift+k" = "previous_tab";
        "ctrl+shift+page_down" = "previous_tab";
      };

      extraConfig = mkIf (activeTheme != null) ''
        include ~/.config/kitty/config/${activeTheme}.conf
      '';
    };

    create.configFile = mkMerge [
      {
        tab-bar = {
          target = "kitty/tab_bar.py";
          source = "${config.snowflake.configDir}/kitty/${activeTheme}-bar.py";
        };
      }

      (mkIf (activeTheme != null) {
        # TODO: Find ONE general nix-automation entry for VictorMono
        kitty-theme = {
          target = "kitty/config/${activeTheme}.conf";
          text = with config.modules.themes; ''
            font_family               VictorMono NF SemiBold
            italic_font               VictorMono NF SemiBold Italic

            bold_font                 VictorMono NF Bold
            bold_italic_font          VictorMono NF Bold Italic

            font_size                 ${builtins.toString font.mono.size}

            foreground                ${colors.main.types.fg}
            background                ${colors.main.types.bg}

            cursor                    ${colors.main.normal.yellow}
            cursor_text_color         ${colors.main.types.bg}

            tab_bar_background        ${colors.main.types.bg}
            active_tab_foreground     ${colors.main.types.bg}
            active_tab_background     ${colors.main.normal.magenta}
            inactive_tab_foreground   ${colors.main.types.fg}
            inactive_tab_background   ${colors.main.types.bg}

            selection_foreground      ${colors.main.types.bg}
            selection_background      ${colors.main.types.highlight}

            color0                    ${colors.main.normal.black}
            color1                    ${colors.main.normal.red}
            color2                    ${colors.main.normal.green}
            color3                    ${colors.main.normal.yellow}
            color4                    ${colors.main.normal.blue}
            color5                    ${colors.main.normal.magenta}
            color6                    ${colors.main.normal.cyan}
            color7                    ${colors.main.normal.white}

            color8                    ${colors.main.bright.black}
            color9                    ${colors.main.bright.red}
            color10                   ${colors.main.bright.green}
            color11                   ${colors.main.bright.yellow}
            color12                   ${colors.main.bright.blue}
            color13                   ${colors.main.bright.magenta}
            color14                   ${colors.main.bright.cyan}
            color15                   ${colors.main.bright.white}
          '';
        };
      })
    ];
  };
}
