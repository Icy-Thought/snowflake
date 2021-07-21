{ config, lib, pkgs, ... }:

let cfg = config.xdg.configHome;

in {
  xdg.configFile."polybar/polybar-tray.conf".source = ./polybar-tray.conf;
  xdg.configFile."polybar/polybar-tray.sh".source = ./polybar-tray.sh;

  services.polybar = {
    enable = true;
    script = builtins.readFile ./polybar-tray.sh;
    config = {
      "color" = {
        ### Active Colors
        bg = "#68000000";
        fg = "#f8f8f2";
        fg-alt = "#9C9C9C";
        trans = "#00000000";
        white = "#F8F8F2";
        black = "#000000";

        ### Material Colors
        red = "#EE4F84";
        pink = "#F48FB1";
        purple = "#BD99FF";
        deep-purple = "#985EFF";
        indigo = "#3949ab";
        blue = "#6498EF";
        light-blue = "#92B6F4";
        cyan = "#24D1E7";
        teal = "#87DFEB";
        green = "#53E2AE";
        light-green = "#A1EFD3";
        lime = "#c0ca33";
        yellow = "#F1FF52";
        light-yellow = "#F1FA8C";
        grey = "#3D4C5F";
        light-gray = "#56687E";
      };

      "global/wm" = {
        margin-bottom = 0;
        margin-top = 0;
      };

      "bar/mainbar0" = {
        "inherit" = "bar/barbase";
        modules-left = "activity search tagdefault";
      };

      "module/workspace0" = {
        type = "custom/script";
        exec = "leftwm-state -w 0 -t ../template.liquid";
        tail = true;
      };

      "bar/mainbar1" = {
        "inherit" = "bar/barbase";
        modules-left = "activity tagdefault";
      };

      "module/workspace1" = {
        type = "custom/script";
        exec = "leftwm-state -w 1 -t ../template.liquid";
        tail = true;
      };

      "bar/barbase" = {
        width = "100%";
        monitor = "\${env:MONITOR}";
        offset-x = 0;
        offset-y = 0;
        height = 35;
        fixed-center = true;
        bottom = false;

        background = "\${color.bg}";
        foreground = "\${color.fg}";

        radius-top = "0.0";
        radius-bottom = "0.0";

        padding = 3;
        module-margin-left = 2;
        module-margin-right = 0;

        font-0 = "JetBrainsMonoMedium Nerd Font:size=20:weight=Medium;3";
        modules-center = "ewmh";
        modules-right =
          "systray vpn=wireguard-wg pipewire network battery backlight sysmenu date";
        enable-ipc = true;
      };

      "settings" = {
        throttle-output = 5;
        throttle-output-for = 10;

        screenchange-reload = false;

        compositing-background = "source";
        compositing-foreground = "over";
        compositing-overline = "over";
        compositing-underline = "over";
        compositing-border = "over";
        pseudo-transparency = false;
      };

      "module/ewmh" = {
        type = "internal/xworkspaces";

        pin-workspaces = false;
        enable-click = true;
        enable-scroll = true;

        icon-0 = "1;";
        icon-1 = "2;";
        icon-2 = "3;";
        icon-3 = "4;";
        icon-4 = "5;輸";
        icon-5 = "6;";
        icon-6 = "7;";
        icon-7 = "8;ﮕ";
        icon-8 = "9;";
        icon-default = "♟";
        format = "<label-state>";

        label-monitor = "%name%";

        label-active = "%icon%";
        label-active-foreground = "#ffffff";
        label-active-background = "#80000000";
        label-active-underline = "#fba922";
        label-active-padding = 2;

        label-occupied = "%icon%";
        label-occupied-underline = "#555555";
        label-occupied-padding = 2;

        label-urgent = "%icon%";
        label-urgent-foreground = "#000000";
        label-urgent-background = "#EE4F84";
        label-urgent-underline = "#9b0a20";
        label-urgent-padding = 4;

        tokenslabel-empty = "%icon%";
        label-empty-foreground = "#55";
        label-empty-padding = 2;

      };

      "module/pulseaudio" = {
        type = "internal/pulseaudio";
        use-ui-max = false;

        format-volume = "<ramp-volume>";

        label-volume = "%percentage%%";

        label-muted = "ﱝ";
        label-muted-foreground = "\${color.fg-alt}";
        label-muted-font = 2;

        ramp-volume-0 = "";
        ramp-volume-1 = "";
        ramp-volume-2 = "";
        ramp-volume-3 = "";

      };

      "module/date" = {
        type = "internal/date";
        interval = "1.0";

        time = " %l:%M %p";
        time-alt = " %A, %B %d, %Y";

        format = "<label>";
        label = "%time%";

      };

      "module/activity" = {
        type = "custom/text";

        content = "";
        content-foreground = "#7ebae4";

        click-left =
          "rofi -no-config -no-lazy-grab -show drun -theme ${cfg}/leftwm/themes/current/rofi/launcher.rasi";
        click-right =
          "rofi -no-config -no-lazy-grab -show drun -theme ${cfg}/leftwm/themes/current/rofi/launcher.rasi";

      };

      "module/sysmenu" = {
        type = "custom/text";

        content = "襤";
        content-foreground = "\${color.fg}";

        click-left = "${cfg}/leftwm/themes/current/rofi/scripts/powermenu.sh";
        click-right = "${cfg}/leftwm/themes/current/rofi/scripts/powermenu.sh";

      };

      "module/network" = {
        type = "internal/network";
        interface = "wlan0";

        interval = "1.0";
        accumulate-stats = true;
        unknown-as-up = true;

        format-connected = "<label-connected>";
        format-disconnected = "<label-disconnected>";
        format-packetloss = "<label-packetloss>";

        label-connected = "";
        label-disconnected = "";
        label-disconnected-foreground = "\${color.red}";
        label-packetloss = "";

      };

      "module/backlight" = {
        type = "internal/backlight";
        card = "amd_backlight";
        enable-scroll = true;

        format = "<ramp>";

        label = "%percentage%%";

        ramp-0 = "";
        ramp-1 = "";
        ramp-2 = "";

        bar-width = 10;
        bar-indicator = "|";
        bar-fill = "─";
        bar-empty = "─";

      };

      "module/battery" = {
        type = "internal/battery";

        full-at = 99;

        battery = "BAT0";
        adapter = "ADP1";

        poll-interval = 5;

        time-format = "%H:%M";
        format-charging = "<animation-charging>";
        format-discharging = "<ramp-capacity>";
        format-full = "<ramp-capacity>";
        label-charging = "Charging %percentage%%";
        label-discharging = "Discharging %percentage%%";
        label-full = "Fully charged";

        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";

        bar-capacity-width = 10;

        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";
        animation-charging-framerate = 750;

        animation-discharging-0 = "";
        animation-discharging-1 = "";
        animation-discharging-2 = "";
        animation-discharging-3 = "";
        animation-discharging-4 = "";
        animation-discharging-framerate = "500";

      };

      "module/systray" = {
        type = "custom/text";

        content = "";
        content-foreground = "\${color.fg}";

        click-left = "${cfg}/leftwm/themes/current/configs/polybar-tray.sh";
        click-right = "${cfg}/leftwm/themes/current/configs/polybar-tray.sh";

        # click-left = "pkill trayer || trayer --edge bottom --distance 60 --align right --widthtype request --transparent true --tint 0x000000 --alpha 102 --iconspacing 20 --padding 10 --margin 375 --expand true";
        # click-right = "pkill trayer || trayer --edge bottom --distance 60 --align right --widthtype request --transparent true --tint 0x000000 --alpha 102 --iconspacing 20 --padding 10 --margin 375 --expand true";

      };

      "module/tagdefault" = {
        type = "custom/text";

        content = "樂";
        content-foreground = "\${color.fg}";

        click-left = "${cfg}/leftwm/themes/current/default-app";
        click-right = "${cfg}/leftwm/themes/current/default-app";
      };

      "module/search" = {
        type = "custom/text";

        content = "";
        content-foreground = "\${color.fg}";
        content-padding = 0;

        click-left = ''
          rofi -show "Global Search" -modi "Global Search":${cfg}/leftwm/themes/current/rofi/scripts/rofi-spotlight.sh -theme ${cfg}/rofi/config.rasi
        '';
        click-right = "${cfg}/.config/leftwm/themes/current/default-app";
      };
    };
  };
}
