{ options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.extensions.input-method;
in with lib; {
  options.modules.desktop.extensions.input-method = with types; {
    enable = mkEnableOption "Enable CJK input method";
    framework = mkOption {
      type = nullOr (enum [ "fcitx" "ibus" ]);
      default = null;
      description = "Choose the desired language-method framework";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.sessionVariables = {
        GLFW_IM_MODULE =
          "ibus"; # https://github.com/kovidgoyal/kitty/issues/403
        GTK_IM_MODULE = "${cfg.framework}";
        QT_IM_MODULE = "${cfg.framework}";
        SDL_IM_MODULE = "${cfg.framework}";
        XMODIFIERS = "@im=${cfg.framework}";
      };
    }

    (mkIf (cfg.framework == "fcitx") {
      i18n.inputMethod = {
        enable = true;
        type = "fcitx5";
        fcitx5.addons = with pkgs; [
          fcitx5-configtool
          fcitx5-chinese-addons
          my.fcitx5-catppuccin
        ];
      };
    })

    (mkIf (cfg.framework == "ibus") {
      i18n.inputMethod = {
        enable = true;
        type = "ibus";
        ibus.engines = [ pkgs.ibus-engines.libpinyin ];
      };

      hm.systemd.user.services.ibus-daemon = {
        Unit = {
          Description = "Intelligent Input Bus";
          Documentation = [ "man:ibus-daemon(1)" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          Environment = [ "DISPLAY=:0" ];
          Restart = "on-failure";

          ExecStart = "${pkgs.ibus}/bin/ibus-daemon --replace --xim";
          ExecReload = "${getExe pkgs.ibus} restart";
          ExecStop = "${getExe pkgs.ibus} exit";
        };
        Install.WantedBy = [ "graphical-session.target" ];
      };
    })
  ]);
}
