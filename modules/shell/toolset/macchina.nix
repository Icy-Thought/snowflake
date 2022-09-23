{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.macchina;
  configDir = config.snowflake.configDir;
  macctive = "Xi";
in {
  options.modules.shell.macchina = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [macchina];

    home.configFile = {
      macchina-init = {
        target = "macchina/macchina.toml";
        text = ''
          interface = "wlan0"
          long_uptime = true
          long_shell = false
          long_kernel = false
          current_shell = true
          physical_cores = true

          # theme = "${macctive}"
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
