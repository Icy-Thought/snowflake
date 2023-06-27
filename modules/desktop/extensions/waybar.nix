{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
  inherit (lib.options) mkEnableOption;

  cfg = config.modules.desktop.extensions.waybar;
in {
  options.modules.desktop.extensions.waybar = {
    enable = mkEnableOption "status-bar for wayland";
  };

  config = mkIf cfg.enable {
    # Allow tray-icons to be displayed:
    hm.services.status-notifier-watcher.enable = true;

    # Launch waybar upon entering way-env:
    hm.systemd.user.services.waybar = {
      Unit = {
        Description = "A bar for your wayland environment";
        PartOf = ["tray.target"];
      };
      Service = {
        Type = "dbus";
        BusName = "org.waybar.Bar";
        ExecStart = "${getExe pkgs.waybar}";
        Restart = "on-failure";
      };
      Install = {WantedBy = ["tray.target"];};
    };

    hm.programs.waybar = {
      enable = true;
      settings = [
        {
          "layer" = "top";
          "position" = "top";
          modules-left = [
            "custom/launcher"
            "wlr/workspaces"
            "temperature"
            "idle_inhibitor"
            "mpd"
          ];
          modules-center = ["clock"];
          modules-right = ["memory" "cpu" "network" "battery" "custom/powermenu" "tray"];
          "custom/launcher" = {
            "format" = " ";
            "on-click" = "rofi -no-lazy-grab -show drun -modi drun";
            "tooltip" = false;
          };

          "wlr/workspaces" = {
            "format" = "{icon}";
            "on-click" = "activate";
            "on-scroll-up" = "hyprctl dispatch workspace e+1";
            "on-scroll-down" = "hyprctl dispatch workspace e-1";
          };
          "idle_inhibitor" = {
            "format" = "{icon}";
            "format-icons" = {
              "activated" = "";
              "deactivated" = "";
            };
            "tooltip" = false;
          };
          "battery" = {
            "interval" = 10;
            "states" = {
              "warning" = 20;
              "critical" = 10;
            };
            "format" = "{icon} {capacity}%";
            "format-icons" = ["" "" "" "" "" "" "" "" ""];
            "format-full" = "{icon} {capacity}%";
            "format-charging" = " {capacity}%";
            "tooltip" = false;
          };
          "clock" = {
            "on-click" = "wallpaper_random";
            "on-click-right" = "killall dynamic_wallpaper || dynamic_wallpaper &";
            "interval" = 1;
            "format" = "{:%I:%M %p  %A %b %d}";
            "tooltip" = true;
            # "tooltip-format"= "{=%A; %d %B %Y}\n<tt>{calendar}</tt>"
            "tooltip-format" = ''
              上午：高数
              下午：Ps
              晚上：Golang
              <tt>{calendar}</tt>'';
          };
          "memory" = {
            "interval" = 1;
            "format" = "﬙ {percentage}%";
            "states" = {"warning" = 85;};
          };
          "cpu" = {
            "interval" = 1;
            "format" = " {usage}%";
          };
          "mpd" = {
            "max-length" = 25;
            "format" = "<span foreground='#bb9af7'></span> {title}";
            "format-paused" = " {title}";
            "format-stopped" = "<span foreground='#bb9af7'></span>";
            "format-disconnected" = "";
            "on-click" = "mpc --quiet toggle";
            "on-click-right" = "mpc ls | mpc add";
            "on-click-middle" = "kitty --class='ncmpcpp' --hold sh -c 'ncmpcpp'";
            "on-scroll-up" = "mpc --quiet prev";
            "on-scroll-down" = "mpc --quiet next";
            "smooth-scrolling-threshold" = 5;
            "tooltip-format" = "{title} - {artist} ({elapsedTime:%M:%S}/{totalTime:%H:%M:%S})";
          };
          "network" = {
            "interval" = 1;
            "format-wifi" = "說 {essid}";
            "format-ethernet" = "  {ifname} ({ipaddr})";
            "format-linked" = "說 {essid} (No IP)";
            "format-disconnected" = "說 Disconnected";
            "tooltip" = false;
          };
          "temperature" = {
            # "hwmon-path"= "${env:HWMON_PATH}";
            #"critical-threshold"= 80;
            "tooltip" = false;
            "format" = " {temperatureC}°C";
          };
          "custom/powermenu" = {
            "format" = "";
            "on-click" = ""; # TODO
            "tooltip" = false;
          };
          "tray" = {
            "icon-size" = 15;
            "spacing" = 5;
          };
        }
      ];
      style = ''
              * {
                font-family: "VictorMono Nerd Font";
                font-size: 12pt;
                font-weight: bold;
                border-radius: 0px;
                transition-property: background-color;
                transition-duration: 0.5s;
              }
              @keyframes blink_red {
                to {
                  background-color: rgb(242, 143, 173);
                  color: rgb(26, 24, 38);
                }
              }
              .warning, .critical, .urgent {
                animation-name: blink_red;
                animation-duration: 1s;
                animation-timing-function: linear;
                animation-iteration-count: infinite;
                animation-direction: alternate;
              }
              window#waybar {
                background-color: transparent;
              }
              window > box {
                margin-left: 5px;
                margin-right: 5px;
                margin-top: 5px;
                background-color: rgb(30, 30, 46);
              }
        #workspaces {
                padding-left: 0px;
                padding-right: 4px;
              }
        #workspaces button {
                padding-top: 5px;
                padding-bottom: 5px;
                padding-left: 6px;
                padding-right: 6px;
              }
        #workspaces button.active {
                background-color: rgb(181, 232, 224);
                color: rgb(26, 24, 38);
              }
        #workspaces button.urgent {
                color: rgb(26, 24, 38);
              }
        #workspaces button:hover {
                background-color: rgb(248, 189, 150);
                color: rgb(26, 24, 38);
              }
              tooltip {
                background: rgb(48, 45, 65);
              }
              tooltip label {
                color: rgb(217, 224, 238);
              }
        #custom-launcher {
                font-size: 20px;
                padding-left: 8px;
                padding-right: 6px;
                color: #7ebae4;
              }
        #mode, #clock, #memory, #temperature,#cpu,#mpd, #idle_inhibitor, #temperature, #backlight, #pulseaudio, #network, #battery, #custom-powermenu, #custom-cava-internal {
                padding-left: 10px;
                padding-right: 10px;
              }
              /* #mode { */
              /* 	margin-left: 10px; */
              /* 	background-color: rgb(248, 189, 150); */
              /*     color: rgb(26, 24, 38); */
              /* } */
        #memory {
                color: rgb(181, 232, 224);
              }
        #cpu {
                color: rgb(245, 194, 231);
              }
        #clock {
                color: rgb(217, 224, 238);
              }
        #idle_inhibitor {
                color: rgb(221, 182, 242);
              }
        #temperature {
                color: rgb(150, 205, 251);
              }
        #backlight {
                color: rgb(248, 189, 150);
              }
        #pulseaudio {
                color: rgb(245, 224, 220);
              }
        #network {
                color: #ABE9B3;
              }
        #network.disconnected {
                color: rgb(255, 255, 255);
              }
        #battery.charging, #battery.full, #battery.discharging {
                color: rgb(250, 227, 176);
              }
        #battery.critical:not(.charging) {
                color: rgb(242, 143, 173);
              }
        #custom-powermenu {
                color: rgb(242, 143, 173);
              }
        #tray {
                padding-right: 8px;
                padding-left: 10px;
              }
        #mpd.paused {
                color: #414868;
                font-style: italic;
              }
        #mpd.stopped {
                background: transparent;
              }
        #mpd {
                color: #c0caf5;
              }
        #custom-cava-internal{
                font-family: "Hack Nerd Font" ;
              }
      '';
    };
  };
}
