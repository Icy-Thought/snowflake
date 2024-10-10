{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.c = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "C/C++ development"; };

  config = mkIf config.modules.develop.c.enable (mkMerge [
    {
      user.packages =
        attrValues { inherit (pkgs) clang cmake ccls clang-tools; };

      hm.programs.vscode.extensions =
        attrValues { inherit (pkgs.vscode-extensions.ms-vscode) cpptools; };
    }

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ]);
}
