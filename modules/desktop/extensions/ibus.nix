{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;

  cfg = config.modules.desktop.extensions.ibus;
in {
  options.modules.desktop.extensions.ibus = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "input-method framework";};

  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "ibus";
      ibus.engines = attrValues {
        inherit (pkgs.ibus-engines) libpinyin;
      };
    };
    environment.variables = {
      GTK_IM_MODULE = "ibus";
      QT_IM_MODULE = "ibus";
      XMODIFIERS = "@im=ibus";
      SDL_IM_MODULE = "ibus";
    };
  };
}
