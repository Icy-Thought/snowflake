{
  inputs,
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.rust;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.rust = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.overlays = with inputs; [rust.overlay];

      user.packages = with pkgs; [
        crate2nix
        rust-bin.beta.latest.default
        unstable.rust-analyzer
      ];

      home.programs.vscode.extensions = with pkgs.vscode-extensions; [
        rust-lang.rust-analyzer
      ];

      env.PATH = [
        "$(${getExe pkgs.yarn} global bin)"
      ];

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
      };
    })

    (mkIf devCfg.enable {
      env = {
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        PATH = ["$CARGO_HOME/bin"];
      };
    })
  ];
}
