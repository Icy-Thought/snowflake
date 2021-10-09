{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.inputMF.ibus;
in {
  options.modules.desktop.inputMF.ibus = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    i18n.inputMethod.enabled = "ibus";
    i18n.inputMethod.ibus.engines = with pkgs.ibus-engines; [
      libpinyin
      hangul
      mozc
    ];
  };
}
