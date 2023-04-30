{ config, options, lib, pkgs, ... }:

let inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.shell = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption false; };

  config = mkMerge [
    (mkIf config.modules.develop.shell.enable {
      user.packages = [ pkgs.shellcheck ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
