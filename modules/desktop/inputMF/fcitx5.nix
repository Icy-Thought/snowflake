{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.fcitx5;
in {
  options.modules.desktop.fcitx5.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    i18n.inputMethod.enabled = "fcitx5";
    i18n.inputMethod.fcitx5.addons = with pkgs; [
      fcitx5-gtk
      fcitx5-configtool
      fcitx5-chinese-addons
      fcitx5-mozc
      # fcitx5-hangul
    ];

    environment.variables.GTK_IM_MODULE = "fcitx";
    environment.variables.QT_IM_MODULE = "fcitx";
    environment.variables.XMODIFIERS = "@im=fcitx";
    environment.variables.SDL_IM_MODULE = "fcitx";

    home.file.".local/share/fcitx5/themes".source =
      (pkgs.callPackage "${config.snowflake}/packages/fcitx5-nord.nix") { };
  };
}
