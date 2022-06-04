{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.htop;
  htop = config.home-manager.users.${config.user.name}.lib.htop;
in {
  options.modules.shell.htop = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.programs.htop = {
      enable = true;

      settings = let
        leftMeters = with htop;
          htop.leftMeters [
            (bar "AllCPUs2")
            (bar "CPU")
            (bar "Memory")
            (bar "Swap")
            (bar "PressureStallIOFull")
            (bar "Battery")
          ];

        rightMeters = with htop;
          htop.rightMeters [
            (text "Hostname")
            (text "Tasks")
            (text "LoadAverage")
            (text "DiskIO")
            (text "NetworkIO")
            (text "Uptime")
            (text "Clock")
          ];
      in
        {
          color_scheme = 0;
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
          sort_key = htop.fields.PERCENT_CPU;

          # By default when in tree view, sort by PID.
          tree_view = false;
          tree_sort_direction = 1;
          tree_sort_key = htop.fields.PID;
          tree_view_always_by_pid = false;

          # The fields in the htop table.
          fields = with htop.fields; [
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
        }
        // leftMeters
        // rightMeters;
    };
  };
}
