# modules/desktop/media/docs.nix

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.appliances.extras.docViewer;
  font = config.modules.themes.font;
  fontStyle = config.modules.themes.fontStyle;
  colors = config.modules.themes.colors;
in {
  options.modules.appliances.extras.docViewer = {
    enable = mkBoolOpt false;
    pdf.enable = mkBoolOpt true;
    ebook.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (mkIf cfg.pdf.enable zathura)
      (mkIf cfg.ebook.enable foliate)
    ];

    homeManager.programs.zathura = mkIf cfg.pdf.enable {
      enable = true;
      options = {
        font = "${font} ${fontStyle} 10";
        selection-clipboard = "clipboard";
        first-page-column = "1:1";
        adjust-open = "width";
        window-title-basename = true;

        # Colorscheme -> modules/themes/default.nix
        default-fg = colors.foreground;
        default-bg = colors.background;

        statusbar-fg = colors.white;
        statusbar-bg = colors.zathuraBlack;

        inputbar-fg = colors.yellow;
        inputbar-bg = colors.zathuraBlack;

        notification-fg = colors.white;
        notification-bg = colors.black;

        notification-error-fg = colors.white;
        notification-error-bg = colors.black;

        notification-warning-fg = colors.red;
        notification-warning-bg = colors.black;

        highlight-active-color = colors.selectionForeground;
        highlight-color = colors.selectionBackground;

        completion-fg = colors.yellow;
        completion-bg = colors.zathuraBlack;

        completion-highlight-fg = colors.zathuraBlack;
        completion-highlight-bg = colors.yellow;

        recolor-lightcolor = colors.zathuraBlack;
        recolor-darkcolor = colors.white;

        recolor = "true";
        recolor-keephue = "true";
      };
    };
  };
}
