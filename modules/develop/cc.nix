{ config, options, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.cc = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "C/C++ development"; };

  config = mkMerge [
    (mkIf config.modules.develop.cc.enable {
      user.packages = attrValues ({
        inherit (pkgs) clang bear gdb cmake;
        inherit (pkgs.llvmPackages) libcxx;
      });

      hm.programs.vscode.extensions =
        attrValues ({ inherit (pkgs.vscode-extensions.ms-vscode) cpptools; });
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
