{ inputs, config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  devCfg = config.modules.develop;
  cfg = devCfg.rust;
in {
  options.modules.develop.rust = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt devCfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.overlays = [ inputs.rust.overlay ];

      user.packages = with pkgs; [ rust-bin.beta.latest.default crate2nix ];

      env.PATH = [ "$(${pkgs.yarn}/bin/yarn global bin)" ];

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
      };
    })

    (mkIf cfg.xdg.enable {
      env = {
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        PATH = [ "$CARGO_HOME/bin" ];
      };
    })
  ];
}
