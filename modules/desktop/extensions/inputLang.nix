{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;

  cfg = config.modules.desktop.extensions.input-method;
in {
  options.modules.desktop.extensions.input-method = let
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) nullOr enum;
  in {
    enable = mkEnableOption "Enable CJK input method";
    framework = mkOption {
      type = nullOr (enum ["fcitx" "ibus"]);
      default = null;
      description = "Choose the desired language-method framework";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      environment.variables = {
        GLFW_IM_MODULE = "ibus"; # https://github.com/kovidgoyal/kitty/issues/403
        GTK_IM_MODULE = "${cfg.framework}";
        QT_IM_MODULE = "${cfg.framework}";
        SDL_IM_MODULE = "${cfg.framework}";
        XMODIFIERS = "@im=${cfg.framework}";
      };
    }

    (mkIf (cfg.framework == "fcitx") {
      i18n.inputMethod = {
        enabled = "fcitx5";
        fcitx5.addons = attrValues {
          inherit
            (pkgs)
            fcitx5-configtool
            fcitx5-chinese-addons
            ;
          inherit (pkgs.my) fcitx5-catppuccin;
        };
      };
    })

    (mkIf (cfg.framework == "ibus") {
      i18n.inputMethod = {
        enabled = "ibus";
        ibus.engines = attrValues {
          inherit
            (pkgs.ibus-engines)
            libpinyin
            typing-booster
            ;
        };
      };

      hm.systemd.user.services.ibus-daemon = {
        Unit = {
          Description = "Intelligent Input Bus";
          Documentation = ["man:ibus-daemon(1)"];
          PartOf = ["graphical-session.target"];
        };

        Service = {
          Environment = ["DISPLAY=:0"];
          Restart = "on-failure";

          ExecStart = "${pkgs.ibus}/bin/ibus-daemon --replace --xim";
          ExecReload = "${pkgs.ibus}/bin/ibus restart";
          ExecStop = "${pkgs.ibus}/bin/ibus exit";
        };
        Install.WantedBy = ["graphical-session.target"];
      };
    })
  ]);
}
