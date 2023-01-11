{ options
, config
, lib
, pkgs
, ...
}:

let
  inherit (lib) mkIf mkMerge mkOption;
  inherit (lib.types) nullOr package;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.distraction.lutris;
  wineCfg = config.modules.desktop.virtual.wine;
in
{
  options.modules.desktop.distraction.lutris = {
    enable = mkBoolOpt false;
    package = mkOption {
      type = nullOr package;
      default = with pkgs; lutris.override { extraLibraries = pkgs: [ jansson ]; };
    };
    league.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs;
        (if wineCfg.enable then [ cfg.package ]
        else [
          cfg.package
          wineWowPackages.fonts
          wineWowPackages.stagingFull
          winetricks
        ]);
    })

    (mkIf (cfg.enable && cfg.league.enable) {
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
