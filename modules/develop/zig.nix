{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.zig = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Zig development"; };

  config = mkIf config.modules.develop.zig.enable (mkMerge [
    { user.packages = attrValues { inherit (pkgs) zig zls; }; }

    (mkIf config.modules.develop.xdg.enable {
      # home.sessionVariables = {
      #   ZIG_GLOBAL_CACHE_DIR = "$TMPDIR/zig-cache";
      # };
    })
  ]);
}
