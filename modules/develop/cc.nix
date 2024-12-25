{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.cc = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "C/C++ development"; };

  config = mkIf config.modules.develop.cc.enable (mkMerge [
    {
      user.packages = attrValues {
        inherit (pkgs) clang clang-tools gdb gnumake cmake meson ninja;
      };
    }

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ]);
}
