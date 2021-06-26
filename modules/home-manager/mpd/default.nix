{ config, ... }:
{
  home.file.".config/mpv/mpv.conf".text = ''
    db_file                 "~/.config/mpd/database"
    log_file                "syslog"
    music_directory         "~/Music"
    auto_update             "yes"

    playlist_directory      "~/.config/mpd/playlists"
    pid_file                "~/.config/mpd/pid"
    state_file              "~/.config/mpd/state"
    sticker_file            "~/.config/mpd/sticker.sql"

    audio_output {
           type             "fifo"
           name             "Visualizer feed"
           path             "/tmp/mpd.fifo"
           format           "44100:16:2"
    }

    audio_output {
         type               "pulse"
         name               "pulse audio"
         device             "pulse"
         mixer_type         "hardware"
    }
  '';
}
