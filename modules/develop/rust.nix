{ inputs, config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop.rust;
in {
  options.modules.develop.rust = {
    enable = mkBoolOpt false;
    enableGlobally = mkBoolOpt true;
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.enableGlobally {
      nixpkgs.overlays = [ inputs.rust.overlay ];

      user.packages = with pkgs; [ rust-bin.beta.latest.default crate2nix ];
      # env.PATH = [ "$(${pkgs.yarn}/bin/yarn global bin)" ];
    })

    {
      env = {
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        PATH = [ "$CARGO_HOME/bin" ];
      };

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
      };
    }
  ]);
}
