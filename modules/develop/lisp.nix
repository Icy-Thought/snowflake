{ config, options, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.develop.lisp;
in {
  options.modules.develop.lisp = let inherit (lib.options) mkEnableOption;
  in { guile.enable = mkEnableOption "Lisp-based language"; };

  config = mkMerge [
    (mkIf cfg.guile.enable {
      user.packages = attrValues {
        inherit (pkgs) guile;
        # guile-colorized ?
      };

      # home.file.guile-conf = {
      #   target = "guile";
      #   text = ''
      #     (use-modules (ice-9 readline)
      #                  (ice-9 colorized))

      #     (activate-readline)
      #     (activate-colorized)
      #   '';
      # };
    })
  ];
}
