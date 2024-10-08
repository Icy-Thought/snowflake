{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.kotlin = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Kotlin development"; };

  config = mkIf config.modules.develop.kotlin.enable (mkMerge [{
    user.packages =
      attrValues { inherit (pkgs) kotlin kotlin-language-server ktlint; };

    hm.programs.vscode.extensions =
      attrValues { inherit (pkgs.vscode-extensions.mathiasfrohlich) kotlin; };
  }]);
}
