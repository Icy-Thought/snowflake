{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge optionals;
  inherit (lib.strings) concatStringsSep;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.toolset.social;
  envProto = config.modules.desktop.envProto;
in {
  options.modules.desktop.toolset.social = {
    base.enable = mkBoolOpt false;
    discord.enable = mkBoolOpt false;
    element.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.base.enable {
      # Enable modules that users ought to have installed by default:
      modules.desktop.toolset.social = {
        discord.enable = true;
        element.enable = true;
      };

      # Install packages that have not been configured:
      user.packages = attrValues ({ inherit (pkgs) signal-desktop tdesktop; });
    })

    (mkIf cfg.element.enable {
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
