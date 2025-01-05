{ options, config, lib, pkgs, ... }:

let cfg = config.modules.hardware.plymouth;
in with lib; {
  options.modules.hardware.plymouth = with types; {
    enable = mkEnableOption "Animate NixOS's boot-up process";
    theme = mkOption {
      type = str;
      default = "black_hud";
      description = "Adi1090x Plymouth theme to apply";
    };
  };

  config = mkIf cfg.enable {
    boot = {
      plymouth = {
        enable = true;
        theme = cfg.theme;
        themePackages = [
          (pkgs.adi1090x-plymouth-themes.override {
            selected_themes = [ cfg.theme ];
          })
        ];
      };

      # :NOTE| Enable slient boot-up process
      consoleLogLevel = 0;
      initrd.verbose = false;
      kernelParams = [
        "quiet"
        "splash"
        "boot.shell_on_fail"
        "loglevel=3"
        "rd.systemd.show_status=false"
        "rd.udev.log_level=3"
        "udev.log_priority=3"
      ];

      # :WARN| Hide OS choice during boot! (Any key -> Cancel)
      loader.timeout = 0;
    };
  };
}
