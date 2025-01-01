{ options, config, lib, pkgs, ... }:

let cfg = config.modules.develop.python;
in with lib; {
  options.modules.develop.python = {
    enable = mkEnableOption "Python development";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = with pkgs; [
        uv
        pyright # pylyzer
        ruff
        (python3.withPackages (pykgs: with pykgs; [ ipython ]))
      ];

      environment.shellAliases = {
        py = "python";
        pip = "uv pip";
        venv = "uv venv";
        ipy = "ipython --no-banner";
        ipylab = "ipython --pylab=qt5 --no-banner";
      };
    }

    (mkIf config.modules.develop.xdg.enable {
      home.sessionVariables = {
        PYTHONPYCACHEPREFIX = "$XDG_CACHE_HOME/python";
        PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/pythonrc";
        PYTHONUSERBASE = "$XDG_DATA_HOME/python";
        PYTHON_HISTORY_FILE = "$XDG_CONFIG_HOME/python/history";

        # :NOTE| Tool related variables
        IPYTHONDIR = "$XDG_CONFIG_HOME/ipython";
        JUPYTER_CONFIG_DIR = "$XDG_CONFIG_HOME/jupyter";
        PIP_CONFIG_FILE = "$XDG_CONFIG_HOME/pip/pip.conf";
        PIP_LOG_FILE = "$XDG_STATE_HOME/pip/log";
        WORKON_HOME = "$XDG_DATA_HOME/virtualenvs";
      };
    })
  ]);
}
