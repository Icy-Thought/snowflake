{ config, pkgs, ... }:
{ 
  programs.ncmpcpp = {
    enable = true;

    # Directories
    # mpdMusicDir = "~/Music";
    settings    = {
      ncmpcpp_directory = "~/.config/ncmpcpp";
      lyrics_directory  = "~/.config/ncmpcpp/lyrics";

      # Behaviour
      autocenter_mode     = "yes";
      centered_cursor     = "yes";
      ignore_leading_the  = "yes";
      
      # Appearance
      playlist_display_mode = "classic";
      enable_window_title   = "yes";
      header_visibility     = "no";
      statusbar_visibility  = "no";
      progressbar_look      = "◈◆◇";
      
      # Song List
      song_list_format    = "$7{%a}|{%D}$8 * $5%t$R $6%l";
      now_playing_prefix  = "$5>> $1";
      
      # Visualizer
      visualizer_fifo_path    = "/tmp/mpd.fifo";
      visualizer_output_name  = "mpd_visualizer";
      visualizer_type         = "spectrum";
      visualizer_look         = "●●";
    };  
  };
}
