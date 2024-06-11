{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.develop.python;
in {
  options.modules.develop.python = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "Python development";};

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = attrValues {
        rich-env = pkgs.python3.withPackages (pyPkgs: with pyPkgs; [rich]);
        inherit (pkgs) rye pyright; # pylyzer
        inherit (pkgs.python3Packages) ipython black isort;
      };

      hm.programs.vscode.extensions = attrValues {
        inherit (pkgs.vscode-extensions.ms-python) python;
        inherit (pkgs.vscode-extensions.ms-toolsai) jupyter;
      };

      environment.shellAliases = {
        py = "python";
        pip = "rye";
        ipy = "ipython --no-banner";
        ipylab = "ipython --pylab=qt5 --no-banner";
      };
    }

    (mkIf config.modules.develop.xdg.enable {
      env = {
        PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/pythonrc";
        PYTHON_HISTORY_FILE = "$XDG_CONFIG_HOME/python/history";
        JUPYTER_CONFIG_DIR = "$XDG_CONFIG_HOME/jupyter";
        IPYTHONDIR = "$XDG_CONFIG_HOME/ipython";
      };

      home.configFile.pythonRC = {
        target = "python/pythonrc";
        text = ''
          def configure_rich_repl():
              try:
                  from rich import print, pretty, traceback
                  from rich.logging import RichHandler
              except ImportError:
                 print("Module 'rich' not available.")

              pretty.install(); traceback.install()

              from logging import getLogger
              getLogger().addHandler(RichHandler())

              print("Rich REPL integration == successful!")


          def configure_comp_repl():
              try:
                  import readline
              except ImportError:
                 print("Module readline not available.")
              else:
                 import rlcompleter
                 readline.parse_and_bind("tab: complete")


          if __name__ == "__main__":
              configure_comp_repl()
              configure_rich_repl()
        '';
      };
    })
  ]);
}
