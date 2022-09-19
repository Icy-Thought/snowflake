{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.btop;
in {
  options.modules.shell.btop = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hm.programs.btop = {
      enable = true;
      settings = {
        force_tty = false;
        vim_keys = true; # Directional keys: "h,j,k,l,g,G"
        update_ms = 1500; # ms (s^{-3})
        temp_scale = "celsius";
        base_10_sizes = false;
        show_cpu_freq = true;
        clock_format = "%H:%M";
        background_update = true;
        disks_filter = "exclude=/boot";

        color_theme = "Default";
        rounded_corners = true;
        theme_background = false;
        truecolor = true;
        presets = builtins.concatStringsSep "," [
          "cpu:1:default"
          "proc:0:default cpu:0:default"
          "mem:0:default"
          "net:0:default cpu:0:block"
          "net:0:tty"
        ];
        graph_symbol = "braille";
        graph_symbol_cpu = "default";
        graph_symbol_mem = "default";
        graph_symbol_net = "default";
        graph_symbol_proc = "default";
        shown_boxes = "proc cpu mem net";

        proc_sorting = "cpu lazy";
        proc_reversed = false;
        proc_tree = false;
        proc_colors = true;
        proc_gradient = true;
        proc_per_core = true;
        proc_mem_bytes = true;
        proc_info_smaps = false;
        proc_left = false;

        cpu_graph_upper = "total";
        cpu_graph_lower = "total";
        cpu_single_graph = false;
        cpu_bottom = false;
        show_uptime = true;
        check_temp = true;
        cpu_sensor = "Auto";
        show_coretemp = true;
        cpu_core_map = "";
        custom_cpu_name = "";

        mem_graphs = true;
        mem_below_net = false;

        show_swap = true;
        swap_disk = true;
        show_disks = true;
        only_physical = true;
        use_fstab = false; # Enable -> disables `only_physical`
        disk_free_priv = false;
        show_io_stat = true;

        io_mode = false;
        io_graph_combined = false;
        io_graph_speeds = "";

        net_download = 100;
        net_upload = 100;
        net_auto = true;
        net_sync = false;
        net_iface = "br0";
        show_battery = true;
        selected_battery = "Auto";
        log_level = "DEBUG";
      };
    };
  };
}
