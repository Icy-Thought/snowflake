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
  options.modules.desktop.extra.rofi = {
    enable = mkBoolOpt false;
  };

  config = mkIf (cfg.xmonad.enable || cfg.qtile.enable) {
    user.packages = with pkgs; [
      rofi-systemd

      (writeScriptBin "rofi" ''
        #!${stdenv.shell}
        exec ${getExe rofi} -terminal kitty -m -1 "$@"
      '')

      # TODO: powermenu + screenshot
      # (makeDesktopItem {
      #   name = "lock-display";
      #   desktopName = "Lock screen";
      #   icon = "system-lock-screen";
      #   exec = "${config.dotfiles.binDir}/zzz";
      # })
    ];
  };
}
