{ config
, options
, lib
, pkgs
, ...
}:

let inherit (lib) concatStringsSep mkIf;
  inherit (lib.my) mkBoolOpt;

  themeCfg = config.modules.themes;
in
{
  options.modules.shell.btop = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.shell.btop.enable {
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

        color_theme = "${themeCfg.active}";
        rounded_corners = true;
        theme_background = false;
        truecolor = true;
        presets = concatStringsSep "," [
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

    home.configFile.btop-theme = mkIf (themeCfg.active != null) {
      target = "btop/themes/${themeCfg.active}.theme";
      text = with themeCfg.colors.main; ''
        theme[main_bg]="${types.bg}"
        theme[main_fg]="${types.fg}"
        theme[title]="${types.fg}"
        theme[hi_fg]="${types.highlight}"
        theme[selected_bg]="${types.border}"
        theme[selected_fg]="${types.bg}"
        theme[inactive_fg]="${bright.black}"
        theme[graph_text]="${bright.yellow}"
        theme[meter_bg]="${bright.black}"
        theme[proc_misc]="${bright.yellow}"
        theme[cpu_box]="${bright.cyan}"
        theme[mem_box]="${bright.green}"
        theme[net_box]="${bright.magenta}"
        theme[proc_box]="${bright.yellow}"
        theme[div_line]="${bright.black}"
        theme[temp_start]="${bright.yellow}"
        theme[temp_mid]="${types.panelbg}"
        theme[temp_end]="${bright.red}"
        theme[cpu_start]="${bright.cyan}"
        theme[cpu_mid]="${types.border}"
        theme[cpu_end]="${bright.green}"
        theme[free_start]="${bright.green}"
        theme[free_mid]="${bright.green}"
        theme[free_end]="${bright.green}"
        theme[cached_start]="${bright.yellow}"
        theme[cached_mid]="${bright.yellow}"
        theme[cached_end]="${bright.magenta}"
        theme[available_start]="${bright.yellow}"
        theme[available_mid]="${bright.yellow}"
        theme[available_end]="${bright.yellow}"
        theme[used_start]="${types.panelbg}"
        theme[used_mid]="${types.panelbg}"
        theme[used_end]="${bright.red}"
        theme[download_start]="${bright.blue}"
        theme[download_mid]="${bright.blue}"
        theme[download_end]="${bright.magenta}"
        theme[upload_start]="${bright.blue}"
        theme[upload_mid]="${bright.blue}"
        theme[upload_end]="${bright.magenta}"
        theme[process_start]="${bright.cyan}"
        theme[process_mid]="${types.border}"
        theme[process_end]="${bright.green}"
      '';
    };
  };
}
