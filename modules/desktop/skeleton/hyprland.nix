{ options, config, inputs, lib, pkgs, ... }:

let
  inherit (inputs) hyprland;
  inherit (builtins) readFile toPath;
  inherit (lib) attrValues mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.desktop.hyprland = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.hyprland.enable {
    modules.desktop = {
      envProto = "wayland";
      toolset.fileBrowse = { nautilus.enable = true; };
      extensions = {
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        dunst.enable = true;
        waybar.enable = true;
        rofi.enable = true;
      };
    };
    modules.shell.scripts = {
      brightness.enable = true;
      microphone.enable = true;
      volume.enable = true;
      screenshot.enable = true; # TODO
    };
    modules.hardware.kmonad.enable = true;

    environment.systemPackages = attrValues ({
      inherit (pkgs)
      # hyprpicker
        imv hyprpaper libnotify playerctl wf-recorder wl-clipboard wlr-randr
        wireplumber;
    });

    services.greetd.settings.initial_session = {
      command = "Hyprland";
      user = "${config.user.name}";
    };

    hm.imports = [ hyprland.homeManagerModules.default ];

    hm.wayland.windowManager.hyprland = {
      enable = true;
      extraConfig =
        readFile "${config.snowflake.configDir}/hyprland/hyprland.conf";
    };

    # System wallpaper:
    home.configFile.hypr-wallpaper =
      let inherit (config.modules.themes) wallpaper;
      in mkIf (wallpaper != null) {
        target = "hypr/hyprpaper.conf";
        text = ''
          preload = ${toPath wallpaper}
          wallpaper = eDP-1,${toPath wallpaper}
          ipc = off
        '';
      };
  };
}
