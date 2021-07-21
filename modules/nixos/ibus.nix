{ config, lib, pkgs, ... }: {

  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ libpinyin hangul mozc ];
  };

}
