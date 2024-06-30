{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.lists) optionals;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.strings) concatStringsSep;

  cfg = config.modules.desktop.toolset.communication;
  desktop = config.modules.desktop;
in {
  options.modules.desktop.toolset.communication = let
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) nullOr enum;
  in {
    base.enable = mkEnableOption "cross-platform clients";
    notmuch.enable = mkEnableOption "NotMuch of an e-mail client";
    discord.enable =
      mkEnableOption "discord client"
      // {
        default = cfg.base.enable;
      };
    matrix = {
      withDaemon = {
        enable =
          mkEnableOption "matrix daemon for ement.el"
          // {
            default = config.modules.desktop.editors.emacs.enable && !cfg.matrix.withClient.enable;
          };
      };
      withClient = {
        enable =
          mkEnableOption "rust-based matrix client"
          // {
            default = cfg.base.enable && !cfg.matrix.withDaemon.enable;
          };
        package = mkOption {
          type = nullOr (enum ["element" "fractal"]);
          default = "element";
          description = "What display protocol to use.";
        };
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.base.enable {
      user.packages = attrValues {
        inherit (pkgs) signal-desktop;
      };
    })

    (mkIf cfg.notmuch.enable {
      hm.accounts.email = {
        maildirBasePath = "${config.hm.xdg.dataHome}/mail";

        accounts.${config.user.name} = let
          mailAddr = "IcyThought@disroot.org";
        in {
          realName = "${config.user.name}";
          userName = "${mailAddr}";
          address = "${mailAddr}";
          passwordCommand = "cat /run/agenix/mailingQST";
          primary = true;

          flavor = "plain";
          imap = {
            host = "disroot.org";
            port = 993;
          };
          smtp = {
            host = "disroot.org";
            port = 465;
          };

          offlineimap.enable = true;
          msmtp = {
            enable = true;
            extraConfig = {auth = "login";};
          };
          notmuch.enable = true;

          gpg = {
            key = "2E690B8644FE29D8237F6C42B593E438DDAB3C66";
            encryptByDefault = false;
            signByDefault = true;
          };
        };
      };

      hm.programs = {
        offlineimap.enable = true;
        msmtp.enable = true;
        notmuch = {
          enable = true;
          hooks = {
            preNew = "${lib.getExe pkgs.offlineimap} -o;";
            postNew = "${lib.getExe pkgs.afew} --tag --new";
          };
          new.tags = ["new"];
        };
        afew.enable = true; # NotMuch initial tagging
      };
    })

    (mkIf cfg.matrix.withDaemon.enable {
      hm.nixpkgs.overlays = [
        (final: prev: {
          pantalaimon = prev.pantalaimon.overrideAttrs (_: {
            version = "10.5-unstable";
            src = pkgs.sources.pantalaimon;
          });
        })
      ];

      hm.services.pantalaimon = {
        enable = true;
        settings = {
          Default = {
            LogLevel = "Debug";
            SSL = true;
          };
          local-matrix = {
            Homeserver = "https://matrix.org";
            ListenAddress = "localhost";
            ListenPort = 8009;
            IgnoreVerification = true;
            UseKeyring = false;
          };
        };
      };
    })

    (mkIf cfg.matrix.withClient.enable {
      user.packages = let
        inherit (pkgs) makeWrapper symlinkJoin element-desktop;
        element-desktop' = symlinkJoin {
          name = "element-desktop-in-dataHome";
          paths = [element-desktop];
          nativeBuildInputs = [makeWrapper];
          postBuild = ''
            wrapProgram "$out/bin/element-desktop" \
              --add-flags '--profile-dir $XDG_DATA_HOME/Element'
          '';
        };
      in
        if (cfg.matrix.withClient.package == "element")
        then [element-desktop']
        else [pkgs.fractal-next];
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
        flags =
          [
            "--flag-switches-begin"
            "--flag-switches-end"
            "--disable-gpu-memory-buffer-video-frames"
            "--enable-accelerated-mjpeg-decode"
            "--enable-accelerated-video"
            "--enable-gpu-rasterization"
            "--enable-native-gpu-memory-buffers"
            "--enable-zero-copy"
            "--ignore-gpu-blocklist"
          ]
          ++ optionals (desktop.type == "x11") [
            "--disable-features=UseOzonePlatform"
            "--enable-features=VaapiVideoDecoder"
          ]
          ++ optionals (desktop.type == "wayland") [
            "--enable-features=UseOzonePlatform,WebRTCPipeWireCapturer"
            "--ozone-platform=wayland"
            "--enable-webrtc-pipewire-capturer"
          ];

        discord-canary' =
          (pkgs.discord-canary.override {withOpenASAR = true;}).overrideAttrs
          (old: {
            preInstall = ''
              gappsWrapperArgs+=("--add-flags" "${concatStringsSep " " flags}")
            '';
          });
      in [discord-canary'];
    })
  ];
}
