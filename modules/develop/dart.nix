{
  inputs,
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.dart;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.dart = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
        dart
        flutter
      ];

      home.programs.vscode.extensions = with pkgs.vscode-utils.extensionsFromVscodeMarketplace; [
        {
          name = "dart-code";
          publisher = "dart-code";
          version = "3.39.20220408";
          sha256 = "D7Pjde4MZDHbYel9YfXowEdy5UNS367Mhg3YHH3ty7c=";
        }
        {
          name = "flutter";
          publisher = "dart-code";
          version = "3.39.20220405";
          sha256 = "OqwzWO8Z/Ql4Y99ki/7/tMFQpltcU5W1cnQc3Wicq+s=";
        }
      ];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
