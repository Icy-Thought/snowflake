{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.distraction.lutris;
  steamCfg = config.modules.desktop.distraction.steam;
  wineCfg = config.modules.desktop.virtual.wine;
in
{
  options.modules.desktop.distraction.lutris = {
    enable = mkBoolOpt false;
    package = mkOption {
      type = with types; nullOr package;
      default = with pkgs; lutris.override { extraLibraries = pkgs: [ jansson ]; };
    };
    league.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf (cfg.enable && wineCfg.enable) {
      user.packages = [ cfg.package ];
    })

    (mkIf (cfg.enable && !wineCfg.enable) {
      user.packages = with pkgs; [
        cfg.package
        wineWowPackages.fonts
        wineWowPackages.stagingFull
        winetricks
      ];
    })

    (mkIf cfg.league.enable {
      boot.kernel.sysctl."abi.vsyscall32" = 0; # anti-cheat...

      networking.firewall.allowedTCPPorts = [ 443 ];

      environment.sessionVariables = {
        QT_X11_NO_MITSHM = "1";
      };

      user.packages = with pkgs; [
        openssl
        gnome.zenity
        vulkan-tools
        dxvk
      ];
    })
  ];
}
