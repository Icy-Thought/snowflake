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
in {
  options.modules.develop.cc = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
        clang
        bear
        gdb
        cmake
        llvmPackages.libcxx
      ];

      home.programs.vscode.extensions = with pkgs.vscode-extensions; [
        ms-vscode.cpptools
      ];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
