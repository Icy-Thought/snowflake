{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) attrValues getExe;
  inherit (lib.modules) mkIf;

  cfg = config.modules.desktop.extensions.rofi;
  envProto = config.modules.desktop.envProto;
in {
  options.modules.desktop.extensions.rofi = let
    inherit (lib.options) mkEnableOption mkPackageOption;
  in {
    enable = mkEnableOption "window switcher and app-launcher";
    package = mkPackageOption pkgs "rofi" {
      default =
        if (envProto == "wayland")
        then "rofi-wayland"
        else "rofi";
    };
  };

  # TODO: re-create theming -> general + changable banner (drun, run, systemd and power-menu)

  config = mkIf cfg.enable {
    user.packages = [pkgs.rofi-systemd];

    hm.programs.rofi = {
      enable = true;
      package = cfg.package;
      plugins = attrValues {inherit (pkgs) rofi-emoji rofi-power-menu;};

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
