{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.distraction.lutris;
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
      networking.firewall.allowedTCPPorts = [ 443 ];

      home.dataFile.wine-ge =
        let
          name = "wine-lutris-ge-lol-7.0-5-x86_64";
          version = "7.0-GE-5-LoL";
        in
        {
          target = "lutris/runners/wine/${name}";
          source = builtins.fetchTarball {
            url = "https://github.com/GloriousEggroll/wine-ge-custom/releases/download/${version}/${name}.tar.xz";
            sha256 = "137sq6az3vnrbwz6gpysh8zd1zv15q5sm7jqqq3vpc4xqlkh53ks";
          };
        };
    })
  ];
}
