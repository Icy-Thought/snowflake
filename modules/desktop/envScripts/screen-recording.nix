{ config, lib, pkgs, ... }:

let
  recordDir = "${config.home.homeDirectory}/Videos/Recordings";

  record-workspace = pkgs.writeScriptBin "record-area" ''
    current=$(date +%Y-%m-%d %H:%M:%S)

    ffmpeg                        \
        -video_size 1920x1080     \
        -framerate 60             \
        -f x11grab                \
        -i :0.0                   \
        -f pulse                  \
        -ac 2                     \
        -i default                \
        -c:v libx264              \
        -pix_fmt yuv420p          \
        -c:a ac3                  \
        -ab 320k                  \
        "${recordDir}/$current.mp4"
  '';

  record-activeWin = pkgs.writeScriptBin "record-area" "";

  record-area = pkgs.writeScriptBin "record-area" ''
    current=$(date +%Y-%m-%d %H:%M:%S)

    hacksaw -n | {
        IFS=+x read -r w h x y

        w=$((w + w % 2))
        h=$((h + h % 2))

        ffmpeg                    \
            -v 16                 \
            -r 30                 \
            -f x11grab            \
            -s "'$'{w}x'$'{h}"    \
            -i ":0.0+$x,$y"       \
            -preset slow          \
            -c:v h264             \
            -pix_fmt yuv420p      \
            -crf 20               \
            "${recordDir}/$current.mp4"
    }
  '';

in { home.packages = [ record-workspace record-activeWin record-area ]; }
