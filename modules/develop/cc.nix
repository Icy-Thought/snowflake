{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop;
in {
  options.modules.develop.cc = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt cfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.cc.enable {
      user.packages = with pkgs; [clang bear gdb cmake llvmPackages.libcxx];
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
