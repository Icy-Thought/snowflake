{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.develop.nix = {
    enable = mkBoolOpt true;
  };

  config = mkMerge [
    (mkIf config.modules.develop.nix.enable {
      user.packages = with pkgs; [
        manix
        nixpkgs-fmt
        nix-output-monitor
        nixpkgs-review
        nil # Nix Expression Language
      ];
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions = with pkgs.vscode-extensions; [ jnoortheen.nix-ide ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
