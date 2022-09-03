{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.nix;
  devCfg = config.modules.develop.xdg;
  codeCfg = config.modules.desktop.editors.vscodium;
in
{
  options.modules.develop.nix = {
    enable = mkBoolOpt true;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
        nix-output-monitor
        nix-review
        rnix-lsp
        nixpkgs-fmt
      ];
    })

    (mkIf codeCfg.enable {
      hm.programs.vscode.extensions = with pkgs.vscode-extensions; [
        jnoortheen.nix-ide
      ];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
