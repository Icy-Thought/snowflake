{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.develop.shell = { enable = mkBoolOpt false; };

  config = mkMerge [
    (mkIf config.modules.develop.shell.enable {
      user.packages = with pkgs; [ shellcheck ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
