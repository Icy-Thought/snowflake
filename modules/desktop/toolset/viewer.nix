{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.toolset.viewer;
in {
  options.modules.desktop.toolset.viewer = {
    zathura.enable = mkBoolOpt false;
    sioyek.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.zathura.enable {
      hm.programs.zathura = {
        enable = true;
        options = with config.modules.themes; (mkMerge [
          {
            adjust-open = "width";
            first-page-column = "1:1";
            window-title-basename = true;
            selection-clipboard = "clipboard";
          }

          (mkIf (active != null) (mkMerge [
            {
              font = "${font.sans.family} ${font.sans.weight} ${
                toString (font.mono.size)
              }";
              recolor = true;
              recolor-keephue = true;
              recolor-reverse-video = true;
            }
            (with colors.main; {
              default-fg = "${types.fg}";
              default-bg = "${types.bg}";

              statusbar-fg = "${normal.white}";
              statusbar-bg = "${types.bg}";

              inputbar-fg = "${normal.yellow}";
              inputbar-bg = "${types.bg}";

              notification-fg = "${normal.white}";
              notification-bg = "${normal.black}";

              notification-error-fg = "${normal.white}";
              notification-error-bg = "${normal.black}";

              notification-warning-fg = "${normal.red}";
              notification-warning-bg = "${normal.black}";

              highlight-active-color = "${types.fg}";
              highlight-color = "${types.highlight}";

              completion-fg = "${normal.yellow}";
              completion-bg = "${types.bg}";

              completion-highlight-fg = "${types.bg}";
              completion-highlight-bg = "${normal.yellow}";

              recolor-lightcolor = "${types.bg}";
              recolor-darkcolor = "${normal.white}";
            })
          ]))
        ]);
      };
    })

    (mkIf cfg.sioyek.enable {
      hm.programs.sioyek = {
        enable = true;
        package = pkgs.sioyek;
        config = {
          "check_for_updates_on_startup" = "0";
          "default_dark_mode" = "1";
          "startup_commands" = builtins.concatStringsSep ";" [
            "toggle_custom_color"
            "toggle_statusbar"
          ];

          "should_launch_new_instance" = "1";
          "sort_bookmarks_by_location" = "1";
          "visual_mark_next_page_fraction" = "0.5";

          "search_url_g" = "https://www.google.com/search?q=";
          "middle_click_search_engine" = "g";

          "ruler_mode" = "1";
          "ruler_padding" = "1.0";
          "ruler_x_padding" = "5.0";
        };

        bindings = {
          # Arrow keys -> hjkl
          "move_up" = "k";
          "move_down" = "j";
          "move_left" = "h";
          "move_right" = "l";

          # Other useful vim-bindings
          "goto_begining" = "gg";
          "goto_end" = "<S-g>";
          "goto_toc" = "<tab>";

          "next_page" = "<S-j>";
          "previous_page" = "<S-k>";
          "prev_state" = "<C-o>";
          "next_state" = "<C-i>";

          "zoom_in" = "=";
          "zoom_out" = "-";
          "fit_to_page_width" = "w";
          "fit_to_page_width_smart" = "e";

          "search" = "/";
          "next_item" = "n";
          "previous_item" = "<S-n>";

          "add_bookmark" = "b";
          "delete_bookmark" = "db";
          "goto_bookmark" = "gb";
          "goto_bookmark_g" = "g<S-b>";

          "add_highlight" = "<S-h>";
          "set_mark" = "m";
          "goto_mark" = "'";

          "link" = "f";
          "delete_link" = "df";
          "quit" = "q";
        };
      };
    })
  ];
}
