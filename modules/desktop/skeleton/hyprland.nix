{ options
, config
, inputs
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let inherit (inputs) hyprland;
in {
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
  };

  imports = [ hyprland.nixosModules.default ];

  config = mkIf config.modules.desktop.xmonad.enable {
    modules.desktop = {
      envProto = "wayland";
      toolset.fileBrowse = {
        nautilus.enable = true;
      };
      extensions = {
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        dunst.enable = true;
        waybar.enable = true;
        rofi = {
          enable = true;
          package = pkgs.rofi-wayland;
        };
      };
    };

    environment.systemPackages = with pkgs; [
      imv
      hyprpaper
      # hyprpicker
      libnotify
      playerctl
      wf-recorder
      wl-clipboard
      wlr-randr
      wireplumber
    ];

    programs.hyprland.enable = true;

    hm.imports = [ hyprland.homeManagerModules.default ];

    hm.wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = builtins.readFile "${config.snowflake.configDir}/hyprland/hyprland.conf"; # TODO
    };

    hm.services = {
      network-manager-applet.enable = true;
      status-notifier-watcher.enable = true;
    };
  };
}
