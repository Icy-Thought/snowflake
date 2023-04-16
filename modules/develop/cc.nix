{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.develop.cc = { enable = mkBoolOpt false; };

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
