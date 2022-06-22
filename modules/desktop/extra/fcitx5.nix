{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop;
in {
  config = mkIf (cfg.xmonad.enable || cfg.qtile.enable) {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [
        fcitx5-configtool
        fcitx5-chinese-addons
        # fcitx5-mozc
        # fcitx5-hangul
      ];
    };

    environment.variables = mkMerge [
      {
        GTK_IM_MODULE = "fcitx";
        QT_IM_MODULE = "fcitx";
        XMODIFIERS = "@im=fcitx";
        SDL_IM_MODULE = "fcitx";
      }

      (mkIf cfg.terminal.kitty.enable {
        GLFW_IM_MODULE = "ibus"; # ibus != fcitx kitty.. bruh
      })
    ];
  };
}
