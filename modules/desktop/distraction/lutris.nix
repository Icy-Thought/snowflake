{ options, config, lib, pkgs, ... }:

let
  inherit (lib) attrValues optionalAttrs mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.distraction.lutris;
  wineCfg = config.modules.desktop.virtual.wine;
in {
  options.modules.desktop.distraction.lutris = {
    enable = mkBoolOpt false;
    league.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = attrValues ({
        lutris =
          pkgs.lutris.override { extraLibraries = pkgs: [ pkgs.jansson ]; };
      } // optionalAttrs (wineCfg.enable == false) {
        inherit (pkgs) winetricks;
        inherit (pkgs.wineWowPackages) fonts stagingFull;
      });
    })

    (mkIf (cfg.enable && cfg.league.enable) {
      boot.kernel.sysctl."abi.vsyscall32" = 0; # anti-cheat...

      networking.firewall.allowedTCPPorts = [ 443 ];

      environment.sessionVariables = { QT_X11_NO_MITSHM = "1"; };

      user.packages = attrValues ({
        inherit (pkgs) openssl vulkan-tools dxvk;
        inherit (pkgs.gnome) zenity;
      });
    })
  ];
}
