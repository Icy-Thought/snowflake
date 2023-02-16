{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.develop.nix = { enable = mkBoolOpt true; };

  config = mkMerge [
    (mkIf config.modules.develop.nix.enable {
      user.packages = attrValues ({
        inherit (pkgs)
          nil # Nix Expression Language
          manix nix-index nix-init nix-output-monitor nix-tree nixfmt
          nixpkgs-review;
      });
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions =
        attrValues ({ inherit (pkgs.vscode-extensions.jnoortheen) nix-ide; });
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
