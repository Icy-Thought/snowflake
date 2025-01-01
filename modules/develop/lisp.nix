{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.develop.lisp = {
    guile.enable = mkEnableOption "Lisp-based language";
  };

  config = mkMerge [
    (mkIf config.modules.develop.lisp.guile.enable {
      user.packages = with pkgs;
        [
          guile
          # guile-colorized ?
        ];

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
