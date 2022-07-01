{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.extra.rofi;
in {
  options.modules.desktop.extra.rofi = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      rofi-systemd

      # TODO: powermenu + screenshot
      # (makeDesktopItem {
      #   name = "lock-display";
      #   desktopName = "Lock screen";
      #   icon = "system-lock-screen";
      #   exec = "${config.dotfiles.binDir}/zzz";
      # })
    ];

    home.programs.rofi = {
      enable = true;
      plugins = with pkgs; [rofi-emoji];

      extraConfig = {
        xoffset = 0;
        yoffset = 0;
        location = "center";

        terminal = "${getExe kitty}";
        disable-history = false;
        sidebar-mode = false;

        sort = true;
        show-icons = true;

        display-drun = "   Run ";
        display-window = " 﩯  Window ";
        display-emoji = "  Emoji ";

        drun-display-format = "{icon} {name}";
        modi = "run,drun,filebrowser,emoji";
      };
    };
  };
}
