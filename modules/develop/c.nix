{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.c = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "C development"; };

  config = mkIf config.modules.develop.c.enable (mkMerge [
    {
      user.packages =
        attrValues { inherit (pkgs) gcc gnumake gdb ninja clang-tools; };
    }

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ]);
}
