{ config, options, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.shell.macchina = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "system-information"; };

  config = mkIf config.modules.shell.macchina.enable {
    user.packages = [ pkgs.macchina ];

    home.configFile = let configDir = config.snowflake.configDir;
    in {
      macchina-init = {
        target = "macchina/macchina.toml";
        text = ''
          interface = "wlan0"
          long_uptime = true
          long_shell = false
          long_kernel = false
          current_shell = true
          physical_cores = true

          # theme = "Xi"
          show = [
              "Machine",
              "Kernel",
              "Distribution",
              "WindowManager",
              "Resolution",
              "Packages",
              "Terminal",
              "Shell",
              "Uptime"
          ]
        '';
      };

      theme-xi = {
        target = "macchina/themes/Xi.conf";
        source = "${configDir}/macchina/xi.toml";
      };

      artwork = {
        target = "macchina/artwork";
        source = "${configDir}/macchina/artwork";
        recursive = true;
      };
    };
  };
}
