{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.meta) getExe;
in {
  options.modules.develop.rust = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Rust development"; };

  config = mkIf config.modules.develop.rust.enable (mkMerge [
    {
      nixpkgs.overlays = [ inputs.rust.overlays.default ];

      user.packages = attrValues {
        rust-package = pkgs.rust-bin.stable.latest.default;
        inherit (pkgs) rust-analyzer rust-script;
      };

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
      };

      hm.programs.vscode.extensions = attrValues {
        inherit (pkgs.vscode-extensions.rust-lang) rust-analyzer;
      };
    }

    (mkIf config.modules.develop.xdg.enable {
      home = {
        sessionVariables.CARGO_HOME = "$XDG_DATA_HOME/cargo";
        sessionPath = [ "$CARGO_HOME/bin" ];
      };
    })
  ]);
}
