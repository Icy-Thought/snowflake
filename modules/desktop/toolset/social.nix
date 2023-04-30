{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues optionals;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.strings) concatStringsSep;

  cfg = config.modules.desktop.toolset.social;
  envProto = config.modules.desktop.envProto;
in {
  options.modules.desktop.toolset.social =
    let inherit (lib.options) mkEnableOption;
    in {
      base.enable = mkEnableOption false;
      discord.enable = mkEnableOption cfg.base.enable;
      element = {
        withDaemon = mkEnableOption false;
        withClient = mkEnableOption cfg.base.enable;
      };
    };

  config = mkMerge [
    (mkIf cfg.base.enable {
      user.packages = attrValues ({ inherit (pkgs) signal-desktop tdesktop; });
    })

    (mkIf cfg.element.withDaemon {
      hm.services.pantalaimon = {
        enable = true;
        settings = {
          Default = {
            LogLevel = "Debug";
            SSL = true;
          };
          local-matrix = {
            Homeserver = "https://matrix.org";
            ListenAddress = "127.0.0.1";
            ListenPort = 8008;
          };
        };
      };
    })

    (mkIf cfg.element.withClient {
      user.packages = let
        inherit (pkgs) makeWrapper symlinkJoin element-desktop;
        element-desktop' = symlinkJoin {
          name = "element-desktop-in-dataHome";
          paths = [ element-desktop ];
          nativeBuildInputs = [ makeWrapper ];
          postBuild = ''
            wrapProgram "$out/bin/element-desktop" \
              --add-flags '--profile-dir $XDG_DATA_HOME/Element'
          '';
        };
      in [ element-desktop' ];
    })

    (mkIf cfg.discord.enable {
      home.configFile.openSAR-settings = {
        target = "discordcanary/settings.json";
        text = builtins.toJSON {
          openasar = {
            setup = true;
            quickstart = true;
            noTyping = false;
            cmdPreset = "balanced";
            css = ''
              @import url("https://catppuccin.github.io/discord/dist/catppuccin-mocha.theme.css");
            '';
          };
          SKIP_HOST_UPDATE = true;
          IS_MAXIMIZED = true;
          IS_MINIMIZED = false;
          trayBalloonShown = true;
        };
      };

      user.packages = let
        flags = [
          "--flag-switches-begin"
          "--flag-switches-end"
          "--disable-gpu-memory-buffer-video-frames"
          "--enable-accelerated-mjpeg-decode"
          "--enable-accelerated-video"
          "--enable-gpu-rasterization"
          "--enable-native-gpu-memory-buffers"
          "--enable-zero-copy"
          "--ignore-gpu-blocklist"
        ] ++ optionals (envProto == "x11") [
          "--disable-features=UseOzonePlatform"
          "--enable-features=VaapiVideoDecoder"
        ] ++ optionals (envProto == "wayland") [
          "--enable-features=UseOzonePlatform,WebRTCPipeWireCapturer"
          "--ozone-platform=wayland"
          "--enable-webrtc-pipewire-capturer"
        ];

        discord-canary' =
          (pkgs.discord-canary.override { withOpenASAR = true; }).overrideAttrs
          (old: {
            preInstall = ''
              gappsWrapperArgs+=("--add-flags" "${concatStringsSep " " flags}")
            '';
          });
      in [ discord-canary' ];
    })
  ];
}
