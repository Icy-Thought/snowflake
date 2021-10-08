{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.htop;
in {
  options.modules.shell.htop.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    programs.htop.enable = true;
    programs.htop.settings = {
      color_scheme = 0;
      enable_mouse = true;
      show_program_path = false;
      sort_direction = 1;
      sort_key = 46;

      left_meters = [ "AllCPUs" "Memory" "Swap" ];
      left_meter_modes = [ 1 1 1 ];
      right_meters = [ "Tasks" "LoadAverage" "Uptime" ];
      left_emter_modes = [ 2 2 2 ];
    };
  };
}
