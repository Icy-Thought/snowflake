{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.python;
  devCfg = config.modules.develop.xdg;
  codeCfg = config.modules.desktop.editors.vscodium;
in {
  options.modules.develop.python = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs;
        [
          python3
          nodePackages.pyright
        ]
        ++ (with python3Packages; [
          # Code-Formatter
          black
          isort

          # Toolset
          ipython
          pip
          poetry
          setuptools
        ]);

      environment.shellAliases = {
        py = "python";
        py2 = "python2";
        py3 = "python3";
        po = "poetry";
        ipy = "ipython --no-banner";
        ipylab = "ipython --pylab=qt5 --no-banner";
      };
    })

    (mkIf codeCfg.enable {
      hm.programs.vscode.extensions = with pkgs.vscode-extensions; [
        ms-python.python
        ms-toolsai.jupyter
      ];
    })

    (mkIf devCfg.enable {
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
