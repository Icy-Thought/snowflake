{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;

  genYAML = pkgs.formats.yaml {};
  deaddDir = "${config.snowflake.configDir}/deadd-notify";
  cfg = config.modules.desktop.extensions.deadd-notify;
in {
  options.modules.desktop.extensions.deadd-notify = let
    inherit (lib.options) literalExpression mkEnableOption mkOption;
  in {
    enable = mkEnableOption "x11 notification center";
    settings = mkOption {
      type = genYAML.type;
      default = {};
      description = ''
        Nix-based deadd-notification-center configuration.
        Please visit the original deadd.conf for determening accepted inputs.
      '';
      example = literalExpression ''
        margin-top: 0
        margin-right: 0
        width: 500
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.packages =
      attrValues {inherit (pkgs) deadd-notification-center;};

    create.configFile = {
      deadd-notify-conf = {
        target = "deadd/deadd.conf";
        source = genYAML.generate "deadd.conf" cfg.settings;
      };
      deadd-notify-css = {
        target = "deadd/deadd.css";
        source = "${deaddDir}/deadd.css";
      };
    };

    # -------===[ Deadd Settings ]===------- #
    modules.desktop.extensions.deadd-notify.settings = {
      notification-center = {
        monitor = 0;
        follow-mouse = false;
        hide-on-mouse-leave = true;
        new-first = true;
        ignore-transient = false;

        ## Minor decorations
        width = 500;

        # Control the margins of our notification
        margin-top = 0;
        margin-right = 0;
        margin-bottom = 0;

        buttons = {
          buttons-per-row = 4;
          button-height = 60;
          button-margin = 2;

          # Define several useful buttons
          actions = [
            {
              label = "VPN";
              command = "sudo vpnToggle";
            }
            {
              label = "Bluetooth";
              command = "bluetoothToggle";
            }
            {
              label = "Wifi";
              command = "wifiToggle";
            }
            {
              label = "Screensaver";
              command = "screensaverToggle";
            }
            {
              label = "Keyboard";
              command = "keyboardToggle";
            }
          ];
        };
      };

      notification = {
        use-markup = true;
        parse-html-entities = true;

        dbus = {send-noti-closed = false;};

        app-icon = {
          guess-icon-from-name = true;
          icon-size = 20;
        };

        image = {
          size = 100;
          margin-top = 15;
          margin-bottom = 15;
          margin-left = 15;
          margin-right = 0;
        };

        modifications = [
          {
            match = {app-name = "Spotify";};
            modify = {
              image-size = 80;
              timeout = 1;
              send-noti-closed = true;
              class-name = "Spotify";
            };
          }
        ];

        popup = {
          default-timeout = 500; # expire in ms

          margin-top = 50;
          margin-right = 50;
          margin-between = 20;
          max-lines-in-body = 3;

          click-behavior = {
            dismiss = "mouse1";
            default-action = "mouse3";
          };
        };
      };
    };
  };
}
