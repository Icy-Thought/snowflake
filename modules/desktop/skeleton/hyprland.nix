{ inputs
, options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.hyprland;
  qtileDir = "${config.snowflake.configDir}/hyprland";
in
{
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
    package = mkOption {
      type = types.package;
      default = pkgs.hyprland;
    };
  };

  config = mkIf cfg.enable {
    # TODO
    nixpkgs.overlays = [ inputs.hyprland.overlays.default ];

    environment.systemPackages = with pkgs; [
      (cfg.package.unwrapped or cfg.package)
      lightdm
      playerctl
      feh
    ];

    # Our beloved modules
    modules.desktop = {
      media.browser.nautilus.enable = true;
      extra = {
        customLayout.enable = true;
        # kmonad.enable = true;
        fcitx5.enable = true;
        mimeApps.enable = true; # mimeApps -> default launch application
        dunst.enable = true;
      };
    };

    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+hyprland";
        lightdm = {
          enable = true;
          greeters.mini.enable = true;
        };
      };
    };

    services = {
      autorandr.enable = true;
      blueman.enable = true;
    };

    hm.services = {
      blueman-applet.enable = true;
      gnome-keyring.enable = true;
      network-manager-applet.enable = true;
      status-notifier-watcher.enable = true;
    };

    hm.xsession = {
      enable = true;
      numlock.enable = true;
      preferStatusNotifierItems = true;
    };
  };
}
