{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.develop.python = { enable = mkBoolOpt false; };

  config = mkMerge [
    (mkIf config.modules.develop.python.enable {
      user.packages = attrValues ({
        inherit (pkgs) python3;
        inherit (pkgs.nodePackages) pyright;
        inherit (pkgs.python3Packages) black isort ipython pip setuptools;
        # poetry
      });

      environment.shellAliases = {
        py = "python";
        py2 = "python2";
        py3 = "python3";
        po = "poetry";
        ipy = "ipython --no-banner";
        ipylab = "ipython --pylab=qt5 --no-banner";
      };
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions = attrValues ({
        inherit (pkgs.vscode-extensions.ms-python) python;
        inherit (pkgs.vscode-extensions.ms-toolsai) jupyter;
      });
    })

    (mkIf config.modules.develop.xdg.enable {
      env = {
        IPYTHONDIR = "$XDG_CONFIG_HOME/ipython";
        PIP_CONFIG_FILE = "$XDG_CONFIG_HOME/pip/pip.conf";
        PIP_LOG_FILE = "$XDG_DATA_HOME/pip/log";
        PYLINTHOME = "$XDG_DATA_HOME/pylint";
        PYLINTRC = "$XDG_CONFIG_HOME/pylint/pylintrc";
        PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/pythonrc";
        PYTHON_EGG_CACHE = "$XDG_CACHE_HOME/python-eggs";
        JUPYTER_CONFIG_DIR = "$XDG_CONFIG_HOME/jupyter";
      };
    })
  ];
}
