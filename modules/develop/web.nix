{ config, options, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.web = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Web development"; };

  config = mkMerge [
    (mkIf config.modules.develop.web.enable {
      user.packages = attrValues {
        inherit (pkgs.nodePackages) typescript typescript-language-server;
      };

      # hm.programs.vscode.extensions =
        # attrValues { inherit (pkgs.vscode-extensions.) ; }; ???
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
