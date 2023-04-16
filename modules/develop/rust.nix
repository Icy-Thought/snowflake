{ inputs, config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge getExe;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.develop.rust = { enable = mkBoolOpt false; };

  config = mkMerge [
    (mkIf config.modules.develop.rust.enable {
      nixpkgs.overlays = [ inputs.rust.overlays.default ];

      user.packages = attrValues ({
        rust-package = pkgs.rust-bin.stable.latest.default;
        inherit (pkgs) crate2nix;
        inherit (pkgs.unstable) rust-analyzer;
      });

      env.PATH = [ "$(${getExe pkgs.yarn} global bin)" ];

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
      };

      hm.programs.vscode.extensions = attrValues ({
        inherit (pkgs.vscode-extensions.rust-lang) rust-analyzer;
      });
    })

    (mkIf config.modules.develop.xdg.enable {
      env = {
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        PATH = [ "$CARGO_HOME/bin" ];
      };
    })
  ];
}
