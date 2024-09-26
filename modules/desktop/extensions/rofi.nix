{ options, config, lib, pkgs, ... }:

let
  inherit (lib.modules) mkIf;
  inherit (lib.strings) concatStringsSep;

  cfg = config.modules.desktop.extensions.rofi;
  desktop = config.modules.desktop;
in {
  options.modules.desktop.extensions.rofi =
    let inherit (lib.options) mkEnableOption mkPackageOption;
    in {
      enable = mkEnableOption "window switcher and app-launcher";
      package = mkPackageOption pkgs "rofi" {
        default =
          if (desktop.type == "wayland") then "rofi-wayland" else "rofi";
      };
    };

  # :TODO| re-create theming -> general + changable banner (drun, run, systemd and power-menu)

  config = mkIf cfg.enable {
    user.packages = [ pkgs.rofi-systemd ];

    hm.programs.rofi = {
      enable = true;
      package = cfg.package;
      plugins = [ pkgs.rofi-power-menu ];

      extraConfig = {
        terminal = "${config.modules.desktop.terminal.default}";
        disable-history = false;
        show-icons = true;
        sidebar-mode = false;
        sort = true;

        drun-display-format = "{icon} {name}";
        display-drun = "   Run ";
        display-window = " 﩯 Window ";
        display-power-menu = "  Power Menu ";

        modi = concatStringsSep "," [
          "run"
          "drun"
          "filebrowser"
          "power-menu:${lib.getExe pkgs.rofi-power-menu}"
        ];

        xoffset = 0;
        yoffset = 0;
      };
    };
  };
}
