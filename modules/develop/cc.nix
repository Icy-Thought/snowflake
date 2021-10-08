{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop.cc;
in {
  options.modules.develop.cc = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ clang gcc bear gdb cmake llvmPackages.libcxx ];
  };
}
