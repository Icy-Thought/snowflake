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
  options.modules.develop.kotlin = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "Kotlin development";};

  config = mkMerge [
    (mkIf config.modules.develop.kotlin.enable {
      user.packages = attrValues {
        inherit (pkgs) kotlin-native kotlin-language-server ktlint;
      };

      hm.programs.vscode.extensions = attrValues {
        inherit (pkgs.vscode-extensions.mathiasfrohlich) kotlin;
      };
    })
  ];
}
