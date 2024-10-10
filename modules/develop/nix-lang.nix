{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.nix = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Nix development" // { default = true; }; };

  config = mkIf config.modules.develop.nix.enable (mkMerge [
    {
      user.packages = attrValues {
        inherit (pkgs)
          nixfmt nil # Nix Expression Language
          manix nix-index nix-init nix-output-monitor nix-tree nix-update
          nixpkgs-review;
      };

      hm.programs.vscode.extensions =
        attrValues { inherit (pkgs.vscode-extensions.jnoortheen) nix-ide; };
    }

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ]);
}
