{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.python = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "Python development";};

  config = mkMerge [
    (mkIf config.modules.develop.python.enable {
      user.packages = attrValues {
        inherit (pkgs) python3 rye;
        inherit (pkgs.nodePackages) pyright;
        inherit (pkgs.python3Packages) black isort ipython;
      };

      environment.shellAliases = {
        py = "python";
        pip = "rye";
        ipy = "ipython --no-banner";
        ipylab = "ipython --pylab=qt5 --no-banner";
      };

      hm.programs.vscode.extensions = attrValues {
        inherit (pkgs.vscode-extensions.ms-python) python;
        inherit (pkgs.vscode-extensions.ms-toolsai) jupyter;
      };
    })

    (mkIf config.modules.develop.xdg.enable {
      env = {
        IPYTHONDIR = "$XDG_CONFIG_HOME/ipython";
        PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/pythonrc";
        JUPYTER_CONFIG_DIR = "$XDG_CONFIG_HOME/jupyter";
      };
    })
  ];
}
