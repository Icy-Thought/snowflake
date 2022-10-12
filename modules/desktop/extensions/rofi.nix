{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.extensions.rofi;
in {
  options.modules.desktop.extensions.rofi = {
    enable = mkBoolOpt false;
    package = mkOption {
      type = types.package;
      default = pkgs.rofi;
      example = "pkgs.rofi-wayland";
    };
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

    hm.programs.rofi = {
      enable = true;
      package = cfg.package;
      plugins = with pkgs; [ rofi-emoji ];

      extraConfig = {
        terminal = with pkgs; "${getExe config.modules.desktop.terminal.default}";
        disable-history = false;
        show-icons = true;
        sidebar-mode = false;
        sort = true;

        display-drun = "   Run ";
        display-emoji = "  Emoji ";
        display-window = " 﩯  Window ";

        drun-display-format = "{icon} {name}";
        modi = "run,drun,filebrowser,emoji";

        xoffset = 0;
        yoffset = 0;
      };
    };
  };
}
