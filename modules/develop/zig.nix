{ inputs, config, options, lib, pkgs, ... }:
let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.zig = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Zig development"; };

  config = mkMerge [
    (mkIf config.modules.develop.zig.enable {
      user.packages = attrValues { inherit (pkgs) zig zls; };

      hm.programs.vscode.extensions =
        attrValues { inherit (pkgs.vscode-extensions.ziglang) vscode-zig; };
    })

    (mkIf config.modules.develop.xdg.enable {
      # home.sessionVariables = {
      #   ZIG_GLOBAL_CACHE_DIR = "$TMPDIR/zig-cache";
      # };
    })
  ];
}
