{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  devCfg = config.modules.develop;
  cfg = devCfg.cc;
in {
  options.modules.develop.cc = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt devCfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [ clang bear gdb cmake llvmPackages.libcxx ];
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
