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
  cfg = config.modules.develop;
in {
  options.modules.develop.rust = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt cfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.rust.enable {
      nixpkgs.overlays = [inputs.rust.overlay];

      user.packages = with pkgs; [
        crate2nix
        rust-bin.beta.latest.default
        unstable.rust-analyzer
      ];

      env.PATH = [
        "$(${getExe pkgs.yarn} global bin)"
      ];

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
      };
    })

    (mkIf cfg.xdg.enable {
      env = {
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        PATH = ["$CARGO_HOME/bin"];
      };
    })
  ];
}
