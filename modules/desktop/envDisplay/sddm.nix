{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.envDisplay.sddm;
in {
  options.modules.desktop.envDisplay.sddm = {
    enable = mkBoolOpt false;
    themeAerial.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.xserver.enable = true;

      services.xserver.displayManager.sddm = {
        enable = true;
        settings = { General.InputMethod = ""; };
      };
    }

    (mkIf (cfg.themeAerial.enable) {
      services.xserver.displayManager.sddm = {
        theme = "${(pkgs.fetchFromGitHub {
          owner = "3ximus";
          repo = "aerial-sddm-theme";
          rev = "2fa0a4024bab60b0ba40de274880e0c1aa6eca59";
          sha256 = "jaGQaClD7Hk4eWh+rMX8ZtcGDzb9aCu+NX5gzJ1JXQg=";
        })}";
      };

      environment.systemPackages = with pkgs; [
        qt5.qtmultimedia
        libsForQt5.qt5.qtgraphicaleffects
      ];
    })
  ]);
}
