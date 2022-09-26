{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.develop.shell = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf config.modules.develop.shell.enable {
      user.packages = with pkgs; [ shellcheck ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
