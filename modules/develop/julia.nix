{ config, options, lib, pkgs, ... }:

let inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.julia = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Julia development"; };

  config = mkMerge [
    (mkIf config.modules.develop.julia.enable {
      user.packages = [ pkgs.julia-bin ];
      # TODO: automate the installation of: [ Gadfly LanguageServer ]
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
