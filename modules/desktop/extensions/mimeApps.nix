{ options, config, lib, ... }:
let
  inherit (builtins) listToAttrs;
  inherit (lib.attrsets) mapAttrsToList nameValuePair;
  inherit (lib.lists) flatten;
  inherit (lib.modules) mkIf;

  cfg = config.modules.desktop.extensions.mimeApps;
in {
  options.modules.desktop.extensions.mimeApps = let
    inherit (lib.options) mkEnableOption;
    inherit (lib.types) str;
    inherit (lib.my) mkOpt;
  in {
    enable = mkEnableOption "default system applications";
    applications = {
      docReader = mkOpt str "org.pwmt.zathura.desktop";
      editor = mkOpt str "emacsclient.desktop";
      fileManager = mkOpt str "org.gnome.Nautilus.desktop";
      imageViewer = mkOpt str "feh.desktop";
      mediaPlayer = mkOpt str "mpv.desktop";
      torrentCli = mkOpt str "transmission-gtk.desktop";
      browser = mkOpt str "zen.desktop";
    };
  };

  config = mkIf cfg.enable {
    xdg.mime = {
      enable = true;
      defaultApplications = let
        applications = let
          inherit (cfg.applications)
            docReader editor fileManager imageViewer mediaPlayer torrentCli
            browser;
        in {
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
      in listToAttrs (flatten (mapAttrsToList (key: types:
        map (type: nameValuePair type (applications."${key}")) types) mimeMap));
    };
  };
}
