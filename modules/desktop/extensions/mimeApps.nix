{ options, config, lib, ... }:

let cfg = config.modules.desktop.extensions.mimeApps;
in with lib; {
  options.modules.desktop.extensions.mimeApps = with types; {
    enable = mkEnableOption "default system applications";
    applications = {
      docReader = my.mkOpt str "org.pwmt.zathura.desktop";
      editor = my.mkOpt str "emacsclient.desktop";
      fileManager = my.mkOpt str "org.gnome.Nautilus.desktop";
      imageViewer = my.mkOpt str "feh.desktop";
      mediaPlayer = my.mkOpt str "mpv.desktop";
      torrentCli = my.mkOpt str "transmission-gtk.desktop";
      browser = my.mkOpt str "zen.desktop";
    };
  };

  config = mkIf cfg.enable {
    xdg.mime = {
      enable = true;
      defaultApplications = let
        applications = with cfg.applications; {
          audio = [ mediaPlayer ];
          browser = [ browser ];
          compression = [ fileManager ];
          directory = [ fileManager ];
          image = [ imageViewer ];
          magnet = [ torrentCli ];
          mail = [ editor ];
          pdf = [ docReader ];
          text = [ editor ];
          video = [ mediaPlayer ];
        };
        mimeMap = {
          audio = builtins.map (x: "audio/" + x) [
            "aac"
            "mpeg"
            "ogg"
            "opus"
            "wav"
            "webm"
            "x-matroska"
          ];
          browser = builtins.map (x: "x-scheme-handler/" + x) [
            "about"
            "http"
            "https"
            "unknown"
          ];
          # calendar = [ "text/calendar" "x-scheme-handler/webcal" ];
          compression = builtins.map (x: "application/" + x) [
            "bzip2"
            "gzip"
            "vnd.rar"
            "x-7z-compressed"
            "x-7z-compressed-tar"
            "x-bzip"
            "x-bzip-compressed-tar"
            "x-compress"
            "x-compressed-tar"
            "x-cpio"
            "x-gzip"
            "x-lha"
            "x-lzip"
            "x-lzip-compressed-tar"
            "x-lzma"
            "x-lzma-compressed-tar"
            "x-tar"
            "x-tarz"
            "x-xar"
            "x-xz"
            "x-xz-compressed-tar"
            "zip"
          ];
          directory = [ "inode/directory" ];
          image = builtins.map (x: "image/" + x) [
            "bmp"
            "gif"
            "jpeg"
            "jpg"
            "png"
            "svg+xml"
            "tiff"
            "vnd.microsoft.icon"
            "webp"
          ];
          magnet = [ "x-scheme-handler/magnet" ];
          mail = [ "x-scheme-handler/mailto" ];
          pdf = [ "application/pdf" ];
          text = [ "text/plain" ];
          video = builtins.map (x: "video/" + x) [
            "mp2t"
            "mp4"
            "mpeg"
            "ogg"
            "webm"
            "x-flv"
            "x-matroska"
            "x-msvideo"
          ];
        };
      in builtins.listToAttrs (flatten (mapAttrsToList (key: types:
        map (type: nameValuePair type (applications."${key}")) types) mimeMap));
    };
  };
}
