{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.htop;
in {
  options.modules.shell.htop = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    homeManager.programs.htop = {
      enable = true;
      settings = {
        color_scheme = 0;
        enable_mouse = true;
        show_program_path = false;
        show_thread_names = true;
        update_process_names = false;

        # The fields in the htop table.
        fields = with config.lib.htop.fields; [
          PID
          USER
          NICE
          IO_PRIORITY
          M_SIZE
          M_RESIDENT
          M_SHARE
          STATE
          PERCENT_CPU
          PERCENT_MEM
          TIME
          STARTTIME
          COMM
        ];

        leftMeters = with config.lib.htop;
          config.lib.htop.leftMeters [
            (bar "AllCPUs2")
            (bar "CPU")
            (bar "Memory")
            (bar "Swap")
            (bar "PressureStallIOFull")
            (bar "Battery")
          ];

        # The meters in the right side of the header.
        rightMeters = with config.lib.htop;
          config.lib.htop.rightMeters [
            (text "Hostname")
            (text "Tasks")
            (text "LoadAverage")
            (text "DiskIO")
            (text "NetworkIO")
            (text "ZFSARC")
            (text "ZFSCARC")
            (text "Uptime")
            (text "Clock")
          ];

        # Show the CPU frequency and usage percentages in the CPU bars.
        show_cpu_frequency = true;
        show_cpu_usage = true;
        cpu_count_from_one = false;
        detailed_cpu_time = true;

        # By default when not in tree view, sort by the CPU usage.
        sort_direction = 0;
        sort_key = config.lib.htop.fields.PERCENT_CPU;

        # By default when in tree view, sort by PID.
        tree_view = false;
        tree_sort_direction = 1;
        tree_sort_key = config.lib.htop.fields.PID;
        tree_view_always_by_pid = false;
      };
    };
  };
}
