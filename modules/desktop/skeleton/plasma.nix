{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.plasma = {
    enable = mkEnableOption "modern desktop environment";
  };

  config = mkIf config.modules.desktop.plasma.enable (mkMerge [
    {
      modules.desktop = {
        type = "wayland";
        extensions.input-method = {
          enable = true;
          framework = "fcitx5";
        };
      };

      services.desktopManager.plasma6 = {
        enable = true;
        enableQt5Integration = false;
      };

      environment.plasma6.excludePackages = with pkgs.kdePackages; [
        konsole
        oxygen
      ];
      programs.dconf.enable = true;
    }

    (mkIf (config.modules.desktop.type == "wayland") {
      environment.sessionVariables = {
        ELECTRON_OZONE_PLATFORM_HINT = "auto";
        NIXOS_OZONE_WL = "1";
        MOZ_ENABLE_WAYLAND = "1";
      };
    })
  ]);
}
