{
  inputs,
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.qtile;
  qtileDir = "${config.snowflake.configDir}/qtile";
in {
  options.modules.desktop.qtile = {
    enable = mkBoolOpt false;
    package = mkOption {
      type = types.package;
      default = pkgs.qtile;
    };
    configFile = mkOption {
      type = with types; nullOr path;
      default = "${qtileDir}/config.py";
      example = literalExpression "./config.py";
    };
    backend = mkOption {
      type = types.enum ["x11" "wayland"];
      default = "x11";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (cfg.package.unwrapped or cfg.package)
      lightdm
      libnotify
      dunst
      pavucontrol
      playerctl
      gxmessage
      xdotool
      xclip
      feh
    ];

    modules.desktop.media.browser = {
      nautilus.enable = true;
      # thunar.enable = true;
    };

    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+qtile";
        lightdm = {
          enable = true;
          greeters.mini.enable = true;
        };
      };

      windowManager.session = [
        {
          name = "qtile";
          start = ''
            ${cfg.package}/bin/qtile start -b ${cfg.backend} \
            ${optionalString (cfg.configFile != null) ''
              --config ${cfg.configFile}
            ''} &
            waitPID=$!
          '';
        }
      ];
    };

    services = {
      autorandr.enable = true;
      blueman.enable = true;
    };

    # Xdg.mimeApps -> application management
    modules.desktop.extra.mimeApps.enable = true;

    home.services = {
      gnome-keyring.enable = true;
      blueman-applet.enable = true;
      status-notifier-watcher.enable = true;
      network-manager-applet.enable = true;
    };

    home.xsession = {
      enable = true;
      numlock.enable = true;
      preferStatusNotifierItems = true;
    };
  };
}
