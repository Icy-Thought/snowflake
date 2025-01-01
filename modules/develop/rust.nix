{ inputs, options, config, lib, pkgs, ... }:

with lib; {
  options.modules.develop.rust = {
    enable = mkEnableOption "Rust development";
  };

  config = mkIf config.modules.develop.rust.enable (mkMerge [
    {
      nixpkgs.overlays = [ inputs.rust.overlays.default ];

      user.packages = with pkgs; [
        rust-bin.stable.latest.default
        rust-analyzer
        rust-script
      ];

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
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
