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
  codeCfg = config.modules.desktop.editors.vscodium;
in {
  options.modules.develop.rust = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.overlays = [inputs.rust.overlays.default];

      user.packages = [
        pkgs.crate2nix
        pkgs.rust-bin.beta.latest.default
        pkgs.unstable.rust-analyzer
      ];

      env.PATH = [
        "$(${getExe pkgs.yarn} global bin)"
      ];

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
      };
    })

    (mkIf codeCfg.enable {
      hm.programs.vscode.extensions = [
        pkgs.vscode-extensions.rust-lang.rust-analyzer
      ];
    })

    (mkIf devCfg.enable {
      env = {
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        PATH = ["$CARGO_HOME/bin"];
      };
    })
  ];
}
