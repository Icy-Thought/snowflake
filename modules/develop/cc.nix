{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.develop.cc = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf config.modules.develop.cc.enable {
      user.packages = with pkgs; [
        clang
        bear
        gdb
        cmake
        llvmPackages.libcxx
      ];
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions = with pkgs.vscode-extensions; [ ms-vscode.cpptools ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
