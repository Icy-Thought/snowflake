{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.toolset.htop = {
    enable = mkEnableOption "system monitor";
  };

  config = mkIf config.modules.shell.toolset.htop.enable {
    hm.programs.htop = {
      enable = true;

      settings = with config.hm.lib.htop.leftMeters;
        {
          enable_mouse = true;
          show_program_path = false;
          show_thread_names = true;
          update_process_names = false;

          # Show the CPU frequency and usage percentages in the CPU bars.
          show_cpu_frequency = true;
          show_cpu_usage = true;
          cpu_count_from_one = false;
          detailed_cpu_time = true;

          # By default when not in tree view, sort by the CPU usage.
          sort_direction = 0;
          sort_key = PERCENT_CPU;

          # By default when in tree view, sort by PID.
          tree_view = false;
          tree_sort_direction = 1;
          tree_sort_key = PID;
          tree_view_always_by_pid = false;

          # The fields in the htop table.
          fields = with fields; [
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
        } // (leftMeters [
          (bar "AllCPUs2")
          (bar "CPU")
          (bar "Memory")
          (bar "Swap")
          (bar "PressureStallIOFull")
          (bar "Battery")
        ]) // (rightMeters [
          (text "Hostname")
          (text "Tasks")
          (text "LoadAverage")
          (text "DiskIO")
          (text "NetworkIO")
          (text "Uptime")
          (text "Clock")
        ]);
    };
  };
}
