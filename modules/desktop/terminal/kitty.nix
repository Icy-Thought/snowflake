{ options, config, lib, pkgs, ... }:

let
  inherit (builtins) concatStringsSep toString;
  inherit (lib.attrsets) mapAttrsToList;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.desktop.terminal.kitty =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption "GPU-accelerated terminal emulator"; };

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
        modify_font = concatStringsSep " "
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

      extraConfig = let inherit (config.modules.themes) active;
      in mkIf (active != null) ''
        include ~/.config/kitty/config/${active}.conf
      '';
    };

    create.configFile = let inherit (config.modules.themes) active;
    in (mkMerge [
      {
        tab-bar = {
          target = "kitty/tab_bar.py";
          source = "${config.snowflake.configDir}/kitty/${active}-bar.py";
        };
      }

      (mkIf (active != null) {
        # TODO: Find ONE general nix-automation entry for VictorMono
        kitty-theme = {
          target = "kitty/config/${active}.conf";
          text = let
            inherit (config.modules.themes.colors.main) bright normal types;
            inherit (config.modules.themes.font.mono) size;
          in ''
            font_family               VictorMono NF SemiBold
            italic_font               VictorMono NF SemiBold Italic

            bold_font                 VictorMono NF Bold
            bold_italic_font          VictorMono NF Bold Italic

            font_size                 ${toString size}

            foreground                ${types.fg}
            background                ${types.bg}

            cursor                    ${normal.yellow}
            cursor_text_color         ${types.bg}

            tab_bar_background        ${types.bg}
            active_tab_foreground     ${types.bg}
            active_tab_background     ${normal.magenta}
            inactive_tab_foreground   ${types.fg}
            inactive_tab_background   ${types.bg}

            selection_foreground      ${types.bg}
            selection_background      ${types.highlight}

            color0                    ${normal.black}
            color1                    ${normal.red}
            color2                    ${normal.green}
            color3                    ${normal.yellow}
            color4                    ${normal.blue}
            color5                    ${normal.magenta}
            color6                    ${normal.cyan}
            color7                    ${normal.white}

            color8                    ${bright.black}
            color9                    ${bright.red}
            color10                   ${bright.green}
            color11                   ${bright.yellow}
            color12                   ${bright.blue}
            color13                   ${bright.magenta}
            color14                   ${bright.cyan}
            color15                   ${bright.white}
          '';
        };
      })
    ]);
  };
}
