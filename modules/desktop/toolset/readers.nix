{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) toString;
  inherit (lib.attrsets) optionalAttrs;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.strings) concatStringsSep;

  cfg = config.modules.desktop.toolset.readers;
in {
  options.modules.desktop.toolset.readers = let
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) nullOr enum;
  in {
    enable = mkEnableOption "A document-viewer for our desktop";
    program = mkOption {
      type = nullOr (enum ["sioyek" "zathura"]);
      default = "zathura";
      description = "which document viewer to install";
    };
  };

  config = mkMerge [
    {
      # :NOTE| Notify system about our document viewer
      modules.desktop.extensions.mimeApps.applications.docReader = cfg.program;
    }

    (mkIf (cfg.program == "zathura") {
      hm.programs.zathura = {
        enable = true;
        options = let
          inherit (config.modules.themes) active;
          inherit (config.modules.themes.colors.main) normal types;
        in
          {
            adjust-open = "width";
            first-page-column = "1:1";
            selection-clipboard = "clipboard";
            statusbar-home-tilde = true;
            window-title-basename = true;
          }
          // optionalAttrs (active != null) {
            font = let
              inherit (config.modules.themes.font) mono sans;
            in "${mono.family} Bold ${toString sans.size}";
            recolor = true;
            recolor-keephue = true;
            recolor-reverse-video = true;

            default-fg = "${types.fg}";
            default-bg = "${types.bg}";

            statusbar-fg = "${types.bg}";
            statusbar-bg = "${types.highlight}";

            inputbar-fg = "${normal.yellow}";
            inputbar-bg = "${types.bg}";

            notification-fg = "${normal.white}";
            notification-bg = "${normal.black}";

            notification-error-fg = "${normal.white}";
            notification-error-bg = "${normal.black}";

            notification-warning-fg = "${normal.red}";
            notification-warning-bg = "${normal.black}";

            highlight-active-color = "${types.fg}";
            highlight-color = "${types.panelbg}";

            completion-fg = "${normal.yellow}";
            completion-bg = "${types.bg}";

            completion-highlight-fg = "${types.bg}";
            completion-highlight-bg = "${normal.yellow}";

            recolor-lightcolor = "${types.bg}";
            recolor-darkcolor = "${normal.white}";
          };
      };
    })

    (mkIf (cfg.program == "sioyek") {
      hm.programs.sioyek = {
        enable = true;
        package = pkgs.sioyek;
        config = {
          "check_for_updates_on_startup" = "0";
          "default_dark_mode" = "1";
          "startup_commands" =
            concatStringsSep ";" ["toggle_custom_color" "toggle_statusbar"];

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
