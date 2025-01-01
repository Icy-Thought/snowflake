{ inputs, options, config, lib, pkgs, ... }:

with lib; {
  options.modules.develop.zig = { enable = mkEnableOption "Zig development"; };

  config = mkIf config.modules.develop.zig.enable (mkMerge [
    { user.packages = with pkgs; [ zig zls ]; }

    (mkIf config.modules.develop.xdg.enable {
      # home.sessionVariables = {
      #   ZIG_GLOBAL_CACHE_DIR = "$TMPDIR/zig-cache";
      # };
    })
  ]);
}
