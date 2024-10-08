{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.desktop.plasma = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "modern desktop environment"; };

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
      services.greetd.settings.initial_session.command = "plasmawayland";

      environment.plasma6.excludePackages =
        attrValues { inherit (pkgs.kdePackages) konsole oxygen; };
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
