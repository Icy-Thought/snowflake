{ config, pkgs, ... }:

let
  imports = [ ./ncmpcpp-ueberzug ];

  cover-art =
    "${config.home.homeDirectory}/.config/ncmpcpp/ncmpcpp-ueberzug/ncmpcpp-cover-art.sh";

  notify = ''
    dunstify "Now Playing ♫" "$(ncmpcpp -q --current-song="{%a - }{%t}|{%f}")"
  '';

in {
  inherit imports;

  programs.ncmpcpp = {
    enable = true;
    package = pkgs.ncmpcpp.override { visualizerSupport = true; };
    mpdMusicDir = "${config.home.homeDirectory}/Music";

    settings = {
      ncmpcpp_directory = "${config.xdg.configHome}/ncmpcpp";
      lyrics_directory = "${config.xdg.configHome}/ncmpcpp/lyrics";

      # Defined Behaviour
      system_encoding = "utf-8";
      regular_expressions = "extended";
      autocenter_mode = "yes";
      centered_cursor = "yes";
      ignore_leading_the = "yes";
      empty_tag_marker = "";

      cyclic_scrolling = "yes";
      header_text_scrolling = "yes";
      jump_to_now_playing_song_at_start = "yes";
      lines_scrolled = "1";

      follow_now_playing_lyrics = "yes";
      default_place_to_search_in = "database";
      display_bitrate = "no";

      selected_item_prefix = "* ";
      discard_colors_if_item_is_selected = "no";
      incremental_seeking = "yes";
      seek_time = "1";

      # Playlist
      playlist_display_mode = "columns";
      playlist_disable_highlight_delay = "0";
      playlist_show_remaining_time = "yes";

      # Appearance
      enable_window_title = "yes";
      titles_visibility = "yes";
      header_visibility = "no";
      browser_display_mode = "columns";
      colors_enabled = "yes";

      main_window_color = "yellow";
      header_window_color = "white";
      volume_color = "green";
      active_window_border = "blue";
      current_item_inactive_column_prefix = "$(cyan)$r";
      current_item_inactive_column_suffix = "$/r$(end)";

      progressbar_color = "black";
      progressbar_elapsed_color = "red";
      progressbar_look = "◈◆◇";

      statusbar_color = "white";
      statusbar_visibility = "yes";

      # Now-playing
      now_playing_prefix = "󰮯 ";
      song_status_format = " $2%a $4⟫$3⟫ $8%t $4⟫$3⟫ $5%b ";

      alternative_header_first_line_format =
        "$0$aqqu$/a {$7%a - $9}{$5%t$9}|{$8%f$9} $0$atqq$/a$9";
      alternative_header_second_line_format = "{{$6%b$9}{ [$6%y$9]}}|{%D}";
      user_interface = "alternative";

      # Song List
      song_columns_list_format =
        "(10)[blue]{l} (30)[green]{a} (30)[magenta]{b} (50)[yellow]{t}";
      song_list_format =
        "{$3%n $9}{$7%a - $9}{$5%t$9}|{$8%f$9}$R{$6  %b$9}{$3 │ %l$9}";

      # Visualizer
      visualizer_data_source = "/tmp/mpd.fifo";
      visualizer_output_name = "mpd_visualizer";
      visualizer_type = "spectrum";
      visualizer_look = "●●";

      # Startup
      startup_screen = "visualizer";
      startup_slave_screen = "playlist";
      startup_slave_screen_focus = "yes";
      locked_screen_width_part = 31;

      # ncmpcpp-ueberzug
      execute_on_song_change = "${cover-art} & ${notify}";
    };
  };
}
