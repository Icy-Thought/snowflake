{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.cc;
  devCfg = config.modules.develop.xdg;
  codeCfg = config.modules.desktop.editors.vscodium;
in {
  options.modules.develop.cc = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = [
        pkgs.clang
        pkgs.bear
        pkgs.gdb
        pkgs.cmake
        pkgs.llvmPackages.libcxx
      ];
    })

    (mkIf codeCfg.enable {
      hm.programs.vscode.extensions = [
        pkgs.vscode-extensions.ms-vscode.cpptools
      ];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
