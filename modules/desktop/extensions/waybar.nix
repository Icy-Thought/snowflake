{ options
, config
, lib
, pkgs
, ...

}:
with lib;
with lib.my;

let cfg = config.modules.desktop.extensions.waybar;
in {
  options.modules.desktop.extensions.waybar = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hm.programs.waybar = {
      enable = true;
      package = pkgs.waybar;
      settings = [{
        height = 48;
        position = "top";

        modules-left = [ "custom/virt-key" "sway/workspaces" "sway/mode" ];
        modules-center = [ "sway/window" ];
        modules-right = [ "idle_inhibitor" "pulseaudio" "network" "sway/language" "battery" "clock" "tray" ];
        modules = {
          "custom/virt-key" = {
            format = "";
            on-click = "virt-keyboard-toggle";
          };
          battery = {
            bat = "BAT0";
          };
          network = {
            format-wifi = "{essid} ({signalStrength}%) ";
            format-ethernet = "{ifname}: {ipaddr}/{cidr} ";
            format-linked = "{ifname} (No IP) ";
            format-disconnected = "Disconnected ⚠";
            format-alt = "{ifname}: {ipaddr}/{cidr}";
          };
        };
      }];
      style = builtins.readFile "${config.snowflake.configDir}/waybar/style.css";
      systemd.enable = true;
    };
  };
}
