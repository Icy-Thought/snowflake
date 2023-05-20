{
  inputs,
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.dart = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "Dart development";};

  config = mkMerge [
    (mkIf config.modules.develop.dart.enable {
      user.packages = attrValues {inherit (pkgs) dart flutter;};

      hm.programs.vscode.extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "dart-code";
          publisher = "dart-code";
          version = "3.39.20220408";
          hash = "sha256-D7Pjde4MZDHbYel9YfXowEdy5UNS367Mhg3YHH3ty7c=";
        }
        {
          name = "flutter";
          publisher = "dart-code";
          version = "3.39.20220405";
          hash = "sha256-OqwzWO8Z/Ql4Y99ki/7/tMFQpltcU5W1cnQc3Wicq+s=";
        }
      ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
