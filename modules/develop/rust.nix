{ inputs
, config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.develop.rust = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf config.modules.develop.rust.enable {
      nixpkgs.overlays = [ inputs.rust.overlays.default ];

      user.packages = with pkgs; [
        crate2nix
        rust-bin.beta.latest.default
        unstable.rust-analyzer
      ];

      env.PATH = [ "$(${getExe pkgs.yarn} global bin)" ];

      environment.shellAliases = {
        rs = "rustc";
        ca = "cargo";
      };
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions = with pkgs.vscode-extensions; [ rust-lang.rust-analyzer ];
    })

    (mkIf config.modules.develop.xdg.enable {
      env = {
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        PATH = [ "$CARGO_HOME/bin" ];
      };
    })
  ];
}
