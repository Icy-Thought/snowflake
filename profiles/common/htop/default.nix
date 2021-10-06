{ config, lib, pkgs, ... }: {

  programs.htop = {
    enable = true;
    settings = {
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
