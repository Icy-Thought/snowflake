{
  options,
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) readFile toPath;
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.hyprland = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "hyped wayland WM";};

  config = mkIf config.modules.desktop.hyprland.enable {
    modules.desktop = {
      envProto = "wayland";
      toolset.fileBrowse = {nautilus.enable = true;};
      extensions = {
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        dunst.enable = true;
        waybar.enable = true;
        elkowar.enable = true;
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

    environment.systemPackages = attrValues {
      inherit (pkgs) imv libnotify playerctl wf-recorder wlr-randr;
    };

    services.greetd.settings.initial_session = {
      command = "Hyprland";
      user = "${config.user.name}";
    };

    hm.imports = let
      inherit (inputs) hyprland;
    in [hyprland.homeManagerModules.default];

    hm.wayland.windowManager.hyprland = {
      enable = true;
      extraConfig =
        readFile "${config.snowflake.configDir}/hyprland/hyprland.conf";
    };

    # System wallpaper:
    home.configFile.hypr-wallpaper = let
      inherit (config.modules.themes) wallpaper;
    in
      mkIf (wallpaper != null) {
        target = "hypr/hyprpaper.conf";
        text = ''
          preload = ${toPath wallpaper}
          wallpaper = eDP-1,${toPath wallpaper}
          ipc = off
        '';
      };
  };
}
