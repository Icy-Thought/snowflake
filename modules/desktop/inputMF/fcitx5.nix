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

    home.file.".local/share/fcitx5/themes".source = pkgs.fetchFromGitHub {
      owner = "icy-thought";
      repo = "fcitx5-catppuccin";
      rev = "3b699870fb2806404e305fe34a3d2541d8ed5ef5";
      sha256 = "hOAcjgj6jDWtCGMs4Gd49sAAOsovGXm++TKU3NhZt8w=";
    };
  };
}
