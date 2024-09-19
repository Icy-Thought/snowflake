{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.plasma = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "modern desktop environment"; };

  config = mkIf config.modules.desktop.plasma.enable {
    modules.desktop = {
      type = "wayland";
      extensions.input-method = {
        enable = true;
        framework = "fcitx5";
      };
    };

    programs.dconf.enable = true;

    services.desktopManager.plasma6 = {
      enable = true;
      enableQt5Integration = false;
    };
    services.displayManager.defaultSession = "plasmawayland";

    environment.plasma6.excludePackages =
      attrValues { inherit (pkgs.kdePackages) konsole oxygen; };
  };
}
