{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.nix;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.nix = {
    enable = mkBoolOpt true;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [alejandra];

      home.programs.vscode.extensions = with pkgs.vscode-extensions; [
        jnoortheen.nix-ide
      ];
    })

    (mkIf devCfg.enable {
      # TODO:
    })
  ];
}
