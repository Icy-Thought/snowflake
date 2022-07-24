{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.viewer;
  themeCfg = config.modules.themes;
in {
  options.modules.desktop.media.viewer = {
    document.enable = mkBoolOpt false;
    music.enable = mkBoolOpt false;
    video.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.document.enable {
      hm.programs.zathura = {
        enable = true;
        options = with themeCfg; (mkMerge [
          {
            adjust-open = "width";
            first-page-column = "1:1";
            window-title-basename = true;
            selection-clipboard = "clipboard";
          }

          (
            mkIf (active != null) {
              font = with themeCfg.font; "${sans.family} ${sans.weight} ${toString (mono.size)}";
              recolor = true;
              recolor-keephue = true;
              recolor-reverse-video = true;
            }
            // (with colors.main; {
              default-fg = "${types.fg}";
              default-bg = "${types.bg}";

              statusbar-fg = "${normal.white}";
              statusbar-bg = "${types.bg}";

              inputbar-fg = "${normal.yellow}";
              inputbar-bg = "${types.bg}";

              notification-fg = "${normal.white}";
              notification-bg = "${normal.black}";

              notification-error-fg = "${normal.white}";
              notification-error-bg = "${normal.black}";

              notification-warning-fg = "${normal.red}";
              notification-warning-bg = "${normal.black}";

              highlight-active-color = "${types.fg}";
              highlight-color = "${types.highlight}";

              completion-fg = "${normal.yellow}";
              completion-bg = "${types.bg}";

              completion-highlight-fg = "${types.bg}";
              completion-highlight-bg = "${normal.yellow}";

              recolor-lightcolor = "${types.bg}";
              recolor-darkcolor = "${normal.white}";
            })
          )
        ]);
      };
    })

    (mkIf cfg.music.enable {
      user.packages = with pkgs; [spotify];
      # TODO: spicetify-cli + activeTheme.
    })

    (mkIf cfg.video.enable {
      user.packages = with pkgs; [mpv-with-scripts mpvc];
    })
  ];
}
