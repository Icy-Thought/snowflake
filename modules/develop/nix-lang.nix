{ config, options, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.nix = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption true; };

  config = mkMerge [
    (mkIf config.modules.develop.nix.enable {
      user.packages = attrValues ({
        inherit (pkgs)
          nil # Nix Expression Language
          manix nix-index nix-init nix-output-monitor nix-tree nixfmt
          nixpkgs-review;
      });

      hm.programs.vscode.extensions =
        attrValues ({ inherit (pkgs.vscode-extensions.jnoortheen) nix-ide; });
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
