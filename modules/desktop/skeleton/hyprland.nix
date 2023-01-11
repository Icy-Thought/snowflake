{ options
, config
, inputs
, lib
, pkgs
, ...
}:

let inherit (inputs) hyprland;
  inherit (builtins) readFile toPath;
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in
{
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
  };

  imports = [ hyprland.nixosModules.default ];

  config = mkIf config.modules.desktop.hyprland.enable {
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

    services.xserver.displayManager = {
      defaultSession = "hyprland";
    };

    programs.hyprland.enable = true;

    hm.imports = [ hyprland.homeManagerModules.default ];

    hm.wayland.windowManager.hyprland = {
      enable = true;
      extraConfig = readFile "${config.snowflake.configDir}/hyprland/hyprland.conf"; # TODO
    };

    # Setting our system wallpaper:
    home.configFile.hypr-wallpaper = {
      target = "hypr/hyprpaper.conf";
      text = ''
        preload = ${toPath ../../themes/tokyonight/assets/zaynstewart-anime-girl-night-alone.png}
        wallpaper = eDP-1,${toPath ../../themes/tokyonight/assets/zaynstewart-anime-girl-night-alone.png}
        ipc = off
      '';
    };
  };
}
