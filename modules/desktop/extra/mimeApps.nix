{
  options,
  config,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.extra.mimeApps;
in {
  options.modules.desktop.extra.mimeApps = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    xdg.mime = {
      enable = true;
      defaultApplications = with lists; let
        defaultApps = {
          audio = ["mpv.desktop"];
          browser = ["firefox.desktop"];
          # calendar = [ "org.gnome.Calendar.desktop" ];
          compression = ["org.gnome.Nautilus.desktop"];
          directory = ["org.gnome.Nautilus.desktop"];
          image = ["feh.desktop"];
          magnet = ["transmission-gtk.desktop"];
          mail = ["firefox.desktop"]; # [ "org.gnome.Geary.desktop" ];
          pdf = ["org.pwmt.zathura-cb.desktop"];
          text = ["neovide.desktop"];
          telegram = ["telegramdesktop.desktop"];
          video = ["mpv.desktop"];
        };
        mimeMap = {
          audio = [
            "audio/aac"
            "audio/mpeg"
            "audio/ogg"
            "audio/opus"
            "audio/wav"
            "audio/webm"
            "audio/x-matroska"
          ];
          browser = [
            "text/html"
            "x-scheme-handler/about"
            "x-scheme-handler/http"
            "x-scheme-handler/https"
            "x-scheme-handler/unknown"
          ];
          # calendar = [ "text/calendar" "x-scheme-handler/webcal" ];
          compression = [
            "application/bzip2"
            "application/gzip"
            "application/vnd.rar"
            "application/x-7z-compressed"
            "application/x-7z-compressed-tar"
            "application/x-bzip"
            "application/x-bzip-compressed-tar"
            "application/x-compress"
            "application/x-compressed-tar"
            "application/x-cpio"
            "application/x-gzip"
            "application/x-lha"
            "application/x-lzip"
            "application/x-lzip-compressed-tar"
            "application/x-lzma"
            "application/x-lzma-compressed-tar"
            "application/x-tar"
            "application/x-tarz"
            "application/x-xar"
            "application/x-xz"
            "application/x-xz-compressed-tar"
            "application/zip"
          ];
          directory = ["inode/directory"];
          image = [
            "image/bmp"
            "image/gif"
            "image/jpeg"
            "image/jpg"
            "image/png"
            "image/svg+xml"
            "image/tiff"
            "image/vnd.microsoft.icon"
            "image/webp"
          ];
          magnet = ["x-scheme-handler/magnet"];
          mail = ["x-scheme-handler/mailto"];
          pdf = ["application/pdf"];
          telegram = ["x-scheme-handler/tg"];
          text = ["text/plain"];
          video = [
            "video/mp2t"
            "video/mp4"
            "video/mpeg"
            "video/ogg"
            "video/webm"
            "video/x-flv"
            "video/x-matroska"
            "video/x-msvideo"
          ];
        };
      in
        listToAttrs (flatten (mapAttrsToList
          (key: types:
            map (type: attrsets.nameValuePair type (defaultApps."${key}"))
            types)
          mimeMap));
    };
  };
}
