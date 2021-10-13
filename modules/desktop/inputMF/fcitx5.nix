{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.inputMF.fcitx5;
in {
  options.modules.desktop.inputMF.fcitx5 = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [
        fcitx5-gtk
        fcitx5-configtool
        fcitx5-chinese-addons
        fcitx5-mozc
        # fcitx5-hangul
      ];
    };

    environment.variables = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
      SDL_IM_MODULE = "fcitx";
    };

    home.file.".local/share/fcitx5/themes".source = "${(pkgs.fetchFromGitHub {
      owner = "tonyfettes";
      repo = "fcitx5-nord";
      rev = "bdaa8fb723b8d0b22f237c9a60195c5f9c9d74d1";
      sha256 = "qVo/0ivZ5gfUP17G29CAW0MrRFUO0KN1ADl1I/rvchE=";
    })}";
  };
}
