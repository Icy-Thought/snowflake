{ config, lib, pkgs, ... }: {

  services.mpd = {
    enable = true;
    dataDir = "${config.xdg.configHome}/mpd";
    dbFile = "${config.xdg.configHome}/mpd/dbFile";

    musicDirectory = "${config.home.homeDirectory}/Music";
    playlistDirectory = "${config.xdg.configHome}/mpd/playlists";

    network = {
      listenAddress = "any";
      port = 6600;
    };

    extraConfig = ''
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

      auto_update             "yes"
    '';
  };
}
