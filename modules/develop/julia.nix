{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.develop.julia = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf config.modules.develop.julia.enable {
      user.packages = with pkgs; [ julia-bin ];
      # TODO: automate the installation of: [ Gadfly LanguageServer ]
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
