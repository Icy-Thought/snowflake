{ options, config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption getExe;
  inherit (lib.types) package;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.extensions.rofi;
  envProto = config.modules.desktop.envProto;
in {
  options.modules.desktop.extensions.rofi = {
    enable = mkBoolOpt false;
    package = mkOption {
      type = package;
      default =
        if (envProto == "wayland") then pkgs.rofi-wayland else pkgs.rofi;
    };
  };

  # TODO: re-create theming -> general + changable banner (drun, run, systemd and power-menu)

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rofi-systemd ];

    hm.programs.rofi = {
      enable = true;
      package = cfg.package;
      plugins = with pkgs; [ rofi-emoji rofi-power-menu ];

      extraConfig = {
        terminal = "${getExe config.modules.desktop.terminal.default}";
        disable-history = false;
        show-icons = true;
        sidebar-mode = false;
        sort = true;

        drun-display-format = "{icon} {name}";

        display-drun = "   Run ";
        display-emoji = "   Emoji ";
        display-window = " 﩯 Window ";
        display-power-menu = "  Power Menu ";

        modi = "run,drun,filebrowser,emoji,power-menu:${
            getExe pkgs.rofi-power-menu
          }";

        xoffset = 0;
        yoffset = 0;
      };
    };
  };
}
