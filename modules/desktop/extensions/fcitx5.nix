{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;

  cfg = config.modules.desktop.extensions.fcitx5;
in {
  options.modules.desktop.extensions.fcitx5 = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "generic input method framework";};

  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = attrValues {
        inherit
          (pkgs)
          fcitx5-configtool
          fcitx5-chinese-addons
          # fcitx5-mozc
          
          # fcitx5-hangul
          
          ;
        inherit (pkgs.my) fcitx5-catppuccin;
      };
    };

    environment.variables = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
      SDL_IM_MODULE = "fcitx";
    };
  };
}
